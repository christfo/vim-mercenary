" mercenary.vim - A mercurial wrapper so awesome, you should give it money
" Maintainer:   Jamie Wong <http://jamie-wong.com/>
" Version:      0.1

" TODO(jlfwong):
"   * Mappings in blame to open up :HGcat or :HGshow
"   * Syntax highlighting for :HGshow
"   * Powerline integration
"   * Better Autocompletion
"   * Error handling
"   * Make :HGcat {rev} % do something reasonable if you're already viewing
"     a file at a specifi revision

if ( empty($MERC_DEV) && exists('g:loaded_mercenary') ) || &cp
  finish
endif

let g:loaded_mercenary = 1
if !exists('g:mercenary_hg_executable')
  let g:mercenary_hg_executable = 'hg'
endif

" VimL Utilities {{{1

function! s:clsinit(properties, cls) abort
  let proto_ref = {}
  for name in keys(a:cls)
    let proto_ref[name] = a:cls[name]
  endfor
  return extend(a:properties, proto_ref, "keep")
endfunction

function! s:shellslash(path)
  if exists('+shellslash') && !&shellslash
    return s:gsub(a:path,'\\','/')
  else
    return a:path
  endif
endfunction

function! s:sub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'')
endfunction

function! s:gsub(str,pat,rep) abort
  return substitute(a:str,'\v\C'.a:pat,a:rep,'g')
endfunction

function! s:shellesc(arg) abort
  if a:arg =~ '^[A-Za-z0-9_/.-]\+$'
    return a:arg
  elseif &shell =~# 'cmd'
    return '"'.s:gsub(s:gsub(a:arg, '"', '""'), '\%', '"%"').'"'
  else
    return shellescape(a:arg)
  endif
endfunction

function! s:mercenary_spec(path) abort
  return matchstr(s:shellslash( a:path ), '\C^mercenary://.\{-\}//\zs.*')
endfunction

function! s:mercenary_method(path) abort
    return matchstr( s:mercenary_spec(a:path), '\C.\{-\}\ze:')
endfunction

function! s:mercenary_args(path) abort
    return split(matchstr( s:mercenary_spec(a:path), '\C:\zs.*'), '//')
endfunction

function! s:warn(str)
  echohl WarningMsg
  echomsg a:str
  echohl None
  let v:warningmsg = a:str
endfunction

" }}1
" Mercenary Utilities {{{1

let s:mercenary_commands = []
function! s:add_command(definition) abort
  let s:mercenary_commands += [a:definition]
endfunction

function! s:extract_hg_root_dir(path) abort
  " Return the absolute path to the root directory of the hg repository, or an
  " empty string if the path is not inside an hg directory.

  " Handle mercenary:// paths as special cases
  if s:shellslash(a:path) =~# '^mercenary://.*//'
    return matchstr(s:shellslash(a:path), '\C^mercenary://\zs.\{-\}\ze//')
  endif

  " Expand to absolute path and strip trailing slashes
  let root = s:shellslash(simplify(fnamemodify(a:path, ':p:s?[\/]$??')))
  let prev = ''

  while root !=# prev
    let dirpath = s:sub(root, '[\/]$', '') . '/.hg'
    let type = getftype(dirpath)
    if type != ''
      " File exists, stop here
      return root
    endif
    let prev = root

    " Move up a directory
    let root = fnamemodify(root, ':h')
  endwhile
  return ''
endfunction

function! s:gen_mercenary_path(method, ...) abort
  let merc_path = 'mercenary://' . s:repo().root_dir . '//' . a:method . ':'
  let merc_path .= join(a:000, '//')
  return merc_path
endfunction

" }}}1
" Repo {{{1

let s:repo_cache = {}
function! s:repo(...)
  " Retrieves a Repo instance. If an argument is passed, it is interpreted as
  " the root directory of the hg repo (i.e. what `hg root` would output if run
  " anywhere inside the repository. Otherwise, the repository containing the
  " file in the current buffer is used.
  if !a:0
    return s:buffer().base().repo()
  endif

  let root_dir = a:1
  if !has_key(s:repo_cache, root_dir)
    let s:repo_cache[root_dir] = s:Repo.new(root_dir)
  endif

  return s:repo_cache[root_dir]
endfunction

let s:Repo = {}
function! s:Repo.new(root_dir) dict abort
  let repo = {
        \"root_dir" : a:root_dir
        \}
  return s:clsinit(repo, self)
endfunction

function! s:Repo.hg_command(...) dict abort
  " Return a full hg command to be executed as a string.
  "
  " All arguments passed are translated into hg commandline arguments.
  let cmd = 'cd ' . self.root_dir
  " HGPLAIN is an environment variable that's supposed to override any settings
  " that will mess with the hg command
  let cmd .= ' && HGPLAIN=1 ' . g:mercenary_hg_executable
  let cmd .= ' ' . join(map(copy(a:000), 's:shellesc(v:val)'), ' ')
  return cmd
endfunction

" }}}1
" Buffer {{{1

let s:buffer_cache = {}
function! s:buffer(...)
  " Retrieves a Buffer instance. If an argument is passed, it is interpreted as
  " the buffer number. Otherwise the buffer number of the active buffer is used.
  let bufnr = a:0 ? a:1 : bufnr('%')

  if ! bufexists( bufnr )
    return {}
  endif

  if !has_key(s:buffer_cache, bufnr)
    let s:buffer_cache[bufnr] = s:Buffer.new(bufnr)
  endif

  return s:buffer_cache[bufnr]
endfunction

function! s:set_common_methods()
  setlocal nowrap nomodified nomodifiable readonly
  nnoremap <script> <silent> <buffer> ?             :call <sid>buffer().dump()<CR>
  nnoremap <script> <silent> <buffer> q             :call <sid>MercClose()<CR>
  " cabbrev  <script> <silent> <buffer> q             call <sid>MercClose()
  " cabbrev  <script> <silent> <buffer> quit          call <sid>MercClose()
  nnoremap <buffer> <silent> s                      :<C-U>exe <SID>BlameShowCS()<CR>
  nnoremap <buffer> <silent> c                      :<C-U>exe <SID>BlameCatCS()<CR>
  nnoremap <buffer> <silent> d                      :<C-U>exe <SID>BlameDiffCS()<CR>
  if s:buffer().method() == "cat"
    execute "setlocal filetype=" . s:buffer().base().getvar("&filetype")
  endif
endfunction

function! s:buffer_cache.vsplit( ... ) dict abort
  let location = a:0 ? a:1 : "leftabove"
  silent execute 'keepalt ' . location . ' vsplit ' 
  let newwinnr = winnr()
  let w:usequit = 1
  wincmd p 
  return newwinnr
endfunction

function! s:buffer_cache.edit( winnr, path, ... ) dict abort
  let options = a:0 ? a:1 : {}

  let parentbuf = s:buffer().bufnr()
  let replacedbufnr = winbufnr( a:winnr )
  execute "silent setlocal ". get( options, "preopt", "" )
  execute a:winnr .'wincmd w'
  let restore = s:buffer().restore_setting()
  let restore.source_bufnr = parentbuf
  let restore.replacedbufnr = replacedbufnr
  silent execute 'edit  ' . a:path
  let nbuf =  s:buffer()
  call nbuf.set_restore( restore )
  call s:set_common_methods()
  execute "silent setlocal ". get( options, "postopt", "" )
  redraw!
  wincmd p 
  return nbuf
endfunction

let s:Buffer = {}
function! s:Buffer.new(number) dict abort
  let buffer = {
        \"_number" : a:number
        \}
  return s:clsinit(buffer, self)
endfunction

function! s:Buffer.close() dict abort
  let tbufnr = self.bufnr()
  execute tbufnr . "buffer"
  if getwinvar( self.winnr(), "usequit", 0 )
    " echom "using quit"
    execute self.winnr() . "wincmd c"
  else
    let replacedbufnr = getbufvar(self.bufnr(), "replacedbufnr", "#" )
    execute replacedbufnr . "buffer"
  endif
  execute "bwipe ". tbufnr
  try 
    execute remove( s:buffer_cache, tbufnr )
  catch
    echom "no such buffer in cache"
  endtry
endfunction

function! s:Buffer.path() dict abort
  return fnamemodify(bufname(self.bufnr()), ":p")
endfunction

function! s:Buffer.relpath() dict abort
  return fnamemodify(self.path(), ':.')
endfunction

function! s:Buffer.bufnr() dict abort
  return self["_number"]
endfunction

function! s:Buffer.winnr() dict abort
  return bufwinnr( self.bufnr() )
endfunction

function! s:Buffer.enable_mercenary_commands() dict abort
  " TODO(jlfwong): This is horribly wrong if the buffer isn't active
  for command in s:mercenary_commands
    exe 'command! -buffer '.command
  endfor
endfunction

function! s:Buffer.getvar(var) dict abort
  return getbufvar(self.bufnr(), a:var)
endfunction

function! s:Buffer.setvar(var, value) dict abort
  return setbufvar(self.bufnr(), a:var, a:value)
endfunction

function! s:Buffer.repo() dict abort
  return s:repo(s:extract_hg_root_dir(self.path()))
endfunction

function! s:Buffer.mercenary_spec() dict abort
  return s:mercenary_spec(self.path() )
endfunction

function! s:Buffer.method() dict abort
  return s:mercenary_method(self.path() )
endfunction

function! s:Buffer.arguments() dict abort
  return s:mercenary_args(self.path() )
endfunction

function! s:Buffer.restore_setting() dict abort
  " Remember the bufnr that :HGblame was invoked in
  let source_bufnr = self.bufnr()

  " Save the settings in the main buffer to be overridden so they can be
  " restored when the buffer is closed
  let restore = ""
  for prop in [ "&scrollbind", "&wrap", "&foldenable" ]
    let origval = self.getvar( prop )
    let restore .= '|call setwinvar(bufwinnr(' . source_bufnr . '), "&wrap", ' . origval . ')'
  endfor
  let restore .= "|" . source_bufnr . 'buffer' 
  return { "restore" : restore, "source_bufnr" : source_bufnr }
endfunction

function! s:Buffer.set_restore( restore_vals ) dict abort
  call self.setvar( "source_bufnr", a:restore_vals["source_bufnr"] )
  call self.setvar( "replacedbufnr", a:restore_vals["replacedbufnr"] )
  let restore_cmd = a:restore_vals["restore"]
  call self.onwinleave( restore_cmd  )
  " When the current buffer containing the blame leaves the window, restore the
  " settings on the source window.
endfunction

function! s:Buffer.parent() dict abort
  let parent = self.getvar("source_bufnr")
  return s:buffer( parent )
endfunction

function! s:Buffer.children() dict abort
  let children = []
  for b in range(1, bufnr('$'))
    if ( bufexists(b) )
      if ( self.bufnr() == getbufvar(b,"source_bufnr"))
        call add( children, b )
      endif
    endif
  endfor
  return children
endfunction

function! s:Buffer.chain() dict abort
  let chain = [self.bufnr()]
  while 1
    let nextparent = getbufvar(chain[-1], "source_bufnr")
    if ( ! nextparent )
      break
    endif
    call add( chain, nextparent )
  endwhile 
  return chain
endfunction

function! s:Buffer.base() dict abort
  return s:buffer( self.chain()[-1] )
endfunction

function! s:Buffer.onwinleave(cmd) dict abort
  call setwinvar( self.winnr(), 'mercenary_bufwinleave', a:cmd )
endfunction

function! s:Buffer_winleave(bufnr) abort
  execute getwinvar(bufwinnr(a:bufnr), 'mercenary_bufwinleave')
endfunction

function! s:Buffer.dump() dict abort
  echom "buffer number " . self.bufnr()
  echom "    keys " . join( keys( self ), ', ')
  echom "    arguments " . join( self.arguments(), ', ')
  echom "    merc spec " . self.mercenary_spec()
  echom "    method " . self.method()
  echom "    path " . self.path()
  echom "    relpath " . self.relpath()
  echom "    rootdir " . s:extract_hg_root_dir( self.path() )
  echom "    children " . join( self.children(), ', ' )
  echom "    chain " . join( self.chain(), ', ')
  echom "    base " . self.base().bufnr() . " - " . self.base().relpath()
  try
    echom "    parent " . self.parent().bufnr()
    echom "    replacedbufnr " . self.getvar("replacedbufnr")
    echom "    repo " . self.repo()
  catch
    echom "    repo : none"
  endtry
  echom "    restore : " string( self.restore_setting() )
  echom "    usequit? " . getwinvar( self.winnr(), "usequit" )
  echom "    winleave " . getwinvar( self.winnr(), 'mercenary_bufwinleave')
endfunction

augroup mercenary_buffer
  autocmd!
  " autocmd BufWinLeave * call s:Buffer_winleave(expand('<abuf>'))
augroup END

function! s:MercMoveSync(direction) "{{{
  while line('.') > 0
    let linetxt = getline('.')
    let cmark = match( linetxt, '^\(| \)*[@o]' )
    if cmark != -1
      call cursor( line('.'), stridx( linetxt, 'o' ) )
      break
    endif
    " need to trap top and bottom"
    let newline = line('.') - a:direction
    if newline > line('$') || newline == 0
      break
    endif
    call cursor( newline, 0 )
  endwhile
endfunction"}}}

function! s:MercMove(direction) range "{{{
  if v:count1 == 0
    let delta = 1 
  else
    let delta = v:count1
  endif
  let seekstep = a:direction
  while delta > 0
    let delta = delta - 1 
    call cursor( line('.') - seekstep, 5 )
    call s:MercMoveSync(a:direction)  
    let seekstep = 3*a:direction
  endwhile
endfunction"}}}

function! s:MercClose(...) "{{{
  let tbuf = a:0 ? s:buffer(a:1) : s:buffer()
  
  for b in tbuf.children()
    if ( b != tbuf.bufnr() && bufexists(b) )
      call s:MercClose(b)
    endif
  endfor

  call tbuf.close()
  redraw!
endfunction "}}}

" }}}1
" Initialization and Routing {{{1
let s:method_handlers = {}

function! s:route(path) abort
  nnoremap <script> <silent> <buffer> ?  :call <sid>buffer().dump()<CR>
  let hg_root_dir = s:extract_hg_root_dir(a:path)
  if hg_root_dir == ''
    return
  endif

  let mercenary_spec = matchstr(s:shellslash(a:path), '\C^mercenary://.\{-\}//\zs.*')

  " echom a:path
  " echom mercenary_spec
  " echom s:mercenary_spec(a:path)
  " echom s:buffer().method()
  
  if s:mercenary_spec(a:path) != ''
    " Route the mercenary:// path
    let method = s:mercenary_method(a:path)
    " echom method

    " Arguments to the mercenary:// methods are delimited by //
    let args = s:mercenary_args(a:path)
    " echom join(args,', ')

    try
      if has_key(s:method_handlers, method)
        call call(s:method_handlers[method], args, s:method_handlers)
      else
        call s:warn('mercenary: unknown mercenary:// method ' . method)
      endif
    catch /^Vim\%((\a\+)\)\=:E118/
      call s:warn("mercenary: Too many arguments to mercenary://" . method)
    catch /^Vim\%((\a\+)\)\=:E119/
      call s:warn("mercenary: Not enough argument to mercenary://" . method)
    endtry
  end

  call s:buffer().enable_mercenary_commands()
endfunction

augroup mercenary
  autocmd!
  autocmd BufNewFile,BufReadPost * call s:route(expand('<amatch>:p'))
augroup END

" }}}1
" :HGblame {{{1

function! s:Blame() abort
  " TODO(jlfwong): hg blame doesn't list uncommitted changes, which can result
  " in misalignment if the file has been modified. Figure out a way to fix this.
  "
  " TODO(jlfwong): Considering switching this to use mercenary://blame

  let hg_args = ['blame', '--changeset', '--number', '--user', '--date', '-q']
  let hg_args += ['--', s:buffer().path()]
  let hg_blame_command = call(s:repo().hg_command, hg_args, s:repo())

  let temppath = resolve(tempname())
  let outfile = temppath . '.mercenaryblame'
  let errfile = temppath . '.err'

  " Write the blame output to a .mercenaryblame file in a temp folder somewhere
  silent! execute '!' . hg_blame_command . ' > ' . outfile . ' 2> ' . errfile

  " Line number of the first visible line in the window + &scrolloff
  let top = line('w0') + &scrolloff
  " Line number of the cursor
  let current = line('.')

  let winnr = s:buffer_cache.vsplit( "leftabove" )
  let blamebuf = s:buffer_cache.edit( winnr, outfile, { 
    \ "preopt" : "scrollbind cursorbind nowrap nofoldenable" ,
    \ "postopt" : "winfixwidth nomodified nospell nomodifiable nonumber scrollbind cursorbind nowrap foldcolumn=0 nofoldenable filetype=mercenaryblame" } )

  execute blamebuf.winnr() . "wincmd w"
  " Synchronize the window position and cursor position between the blame buffer
  " and the code buffer.
  " Execute the line number as a command, focusing on that line (e.g. :23) to
  " synchronize the buffer scroll positions.
  execute top
  normal! zt
  " Synchronize the cursor position.
  execute current
  syncbind

  " Resize the window so we show all of the blame information, but none of the
  " code (the code is shown in the editing buffer :HGblame was invoked in).
  let blame_column_count = strlen(matchstr(getline('.'), '[^:]*:')) - 1
  execute "vertical resize " . blame_column_count
  wincmd p

endfunction

function! s:BlameGetCS() abort
  let csnum = matchstr( getline('.'), '[^0-9]\+\zs\d\+\ze')
  return csnum
endfunction

function! s:BlameSync() abort
  let result = {
    \  'source' : s:buffer(),
    \  'csnum' : s:BlameGetCS(),
    \  'path' : s:buffer().base().relpath(),
  \}
  
  " how to decide where to put non-split edits... 
  "  1) any visible window that matches the base()
  "  2) any visible window that is a current child()
  "  3) any visible  window that is not itself
  "  split.
  "
  let twin = s:buffer().base().winnr()
  if twin 
    let result.target_win = twin
  else
    let children = s:buffer().children()
    let visible_children = filter( children, bufwinnr(v:val) )
    try
      let result.target_win = visible_children[0]
    catch
      let visible = range( 1, winnr('$') )
      " any merc.method()
      "TODO
      " next visible one
      call remove( visible, s:buffer().winnr() )
      let result.target_win = visible_children[0]
    endtry
  endif
  return result
endfunction

function! s:BlameCatCS() abort
  let sync = s:BlameSync()
  if -1 != sync.csnum
    call s:buffer_cache.edit( sync.target_win, s:gen_mercenary_path('cat', sync.csnum, sync.source.base().relpath() ) )
  else 
    echom "Could not find repository reference on this line."
  endif
endfunction

function! s:BlameDiffCS() abort
  let sync = s:BlameSync()
  if -1 != sync.csnum
    call s:DoDiff( sync.target_win, sync.source.base(), sync.csnum )
  else 
    echom "Could not find repository reference on this line."
  endif
endfunction

function! s:BlameShowCS() abort
  let sync = s:BlameSync()
  if -1 != sync.csnum
    call s:buffer_cache.edit( sync.target_win, s:gen_mercenary_path('show', sync.csnum) )
  else 
    echom "Could not find repository reference on this line."
  endif
endfunction

call s:add_command("HGblame call s:Blame()")

augroup mercenary_blame
  autocmd!
  autocmd BufReadPost *.mercenaryblame setfiletype mercenaryblame
augroup END

" }}}1
" :HGcat {{{1

function! s:Cat(rev, ...) abort
  let path = a:0 ? a:1 : s:buffer().path()
  call s:buffer_cache.edit( winnr(), s:gen_mercenary_path('cat', a:rev, path) )
endfunction

call s:add_command("-nargs=+ -complete=file HGcat call s:Cat(<f-args>)")

" }}}1
" mercenary://root_dir//cat:rev//filepath {{{1
function! s:method_handlers.cat(rev, filepath) dict abort
  " TODO(jlfwong): Error handling - (file not found, rev not fond)

  let args = ['cat', '--rev', a:rev, a:filepath]
  let hg_cat_command = call(s:repo().hg_command, args, s:repo())

  let temppath = resolve(tempname())
  let outfile = temppath . '.out'
  let errfile = temppath . '.err'

  silent! execute '!' . hg_cat_command . ' > ' . outfile . ' 2> ' . errfile

  silent! execute 'read ' . outfile
  " :read dumps the output below the current line - so delete the first line
  " (which will be empty)
  let winview = winsaveview()
  let current = line('.')
  0d
  call winrestview( winview )
  execute current

endfunction
" }}}1

" }}}1
" :HGshow {{{1

function! s:Show(rev) abort
  call s:buffer_cache.edit( winnr(), s:gen_mercenary_path('show', a:rev) )
endfunction

call s:add_command("-nargs=1 HGshow call s:Show(<f-args>)")


" }}}1
" mercenary://root_dir//show:rev {{{1
function! s:method_handlers.show(rev) dict abort
  " TODO(jlfwong): DRY this up w/ method_handlers.cat

  let args = ['log', '--stat', '-vpr', a:rev]
  let hg_log_command = call(s:repo().hg_command, args, s:repo())

  let temppath = resolve(tempname())
  let outfile = temppath . '.out'
  let errfile = temppath . '.err'

  silent! execute '!' . hg_log_command . ' > ' . outfile . ' 2> ' . errfile

  silent! execute 'read ' . outfile
  let winview = winsaveview()
  0d
  call winrestview( winview )

  setlocal filetype=diff
endfunction

" }}}1
" :HGlog {{{1
function! s:HGlog(...) abort
  if a:0 == 0
    let file=s:buffer().relpath()
  elseif a:0 == 1
    let file=a:1
  end
  let winnr = s:buffer_cache.vsplit()
  call s:buffer_cache.edit( winnr, s:gen_mercenary_path('glog', file) )
endfunction

call s:add_command("-nargs=? -complete=file HGlog call s:HGlog(<f-args>)")

function! s:GlogMap() 
  nnoremap <buffer> q :silent bd!<CR>
  nnoremap <script> <silent> <buffer> <CR>          :call <sid>BlameShowCS()<CR>
  nnoremap <script> <silent> <buffer> k             :call <sid>MercMove(1)<CR>
  nnoremap <script> <silent> <buffer> j             :call <sid>MercMove(-1)<CR>
  nnoremap <script> <silent> <buffer> <down>        :call <sid>MercMove(-1)<CR>
  nnoremap <script> <silent> <buffer> <up>          :call <sid>MercMove(1)<CR>
  nnoremap <script> <silent> <buffer> <C-U>         <C-U>:call <sid>MercMove(1)<CR>
  nnoremap <script> <silent> <buffer> <C-D>         <C-D>:call <sid>MercMove(-1)<CR>
  nnoremap <script> <silent> <buffer> gg            gg:call <sid>MercMove(1)<CR>
  nnoremap <script> <silent> <buffer> G             G:call <sid>MercMove(-1)<CR>
  " nnoremap <script> <silent> <buffer> P             :call <sid>MercPlayTo()<CR>
  " nnoremap <script> <silent> <buffer> p             :call <sid>MercRenderChangePreview()<CR>
  " nnoremap <script> <silent> <buffer> r             :call <sid>MercRenderPreview()<CR>
  " nnoremap <script> <silent> <buffer> <2-LeftMouse> :call <sid>MercMouseDoubleClick()<CR>
endfunction

function! s:GlogSyntax() 
  let b:current_syntax = 'glog'

  syn match GlogCurrentLocation '@'
  syn match GlogCs 'cs:'
  syn match GlogSum 'summary:'
  syn match GlogHelp '\v^".*$'
  syn match GlogNumberField '\v\[[0-9a-f:]+\]'
  syn match GlogHash '\v[0-9]+' contained containedin=GlogNumberField
  syn match GlogNumber '\v[0-9a-f]{12}' contained containedin=GlogNumberField

  hi def link GlogCurrentLocation Keyword
  hi def link GlogCs Keyword
  hi def link GlogSum Keyword
  hi def link GlogHelp Comment
  hi def link GlogNumberField label
  hi def link GlogNumber Identifier
  hi def link GlogHash Identifier
endfunction

function! s:GlogSettings() 
  setlocal nospell 
  setlocal nomodified 
  setlocal nomodifiable
  setlocal readonly 
  setlocal noswapfile
  setlocal nobuflisted
  setlocal nolist
  setlocal nonumber
  setlocal norelativenumber
  setlocal nowrap
  setlocal bufhidden=
  setlocal buftype=nofile
  setlocal filetype=glog
  call s:GlogSyntax()
  call s:GlogMap()
endfunction 

" }}}1
" mercenary://root_dir//glog:filepath {{{1
function! s:method_handlers.glog(filepath) dict abort 
  let args = ['glog', '--template', 'cs:      [{rev}:{node|short}] {tags}\nsummary: {desc|firstline|fill68|tabindent|tabindent}\n\n', a:filepath]
  let hg_glog_cmd = call(s:repo().hg_command, args, s:repo())
  let temppath = resolve(tempname())
  let outfile = temppath . '.glog'
  let errfile = temppath . '.err'
  silent! execute '!'.hg_glog_cmd.' > '.outfile.' 2> '.errfile
  silent! execute 'read '.outfile

  call s:GlogSettings()

endfunction 

augroup MercGlogAug 
  autocmd!
  autocmd BufReadPost *.glog setfiletype glog
  autocmd Syntax glog call s:GlogSyntax()
augroup END 


" }}}1
" :HGgrep {{{1
function! s:HGgrep(...) abort
  let searchregex = a:0        ? a:1 : expand("<cword>")
  let path        = (a:0 == 2) ? a:2 : s:buffer().relpath()
  let winnr       = s:buffer_cache.vsplit()
  call s:buffer_cache.edit( winnr, s:gen_mercenary_path('grep', searchregex, path ) )
endfunction

call s:add_command("-nargs=? HGgrep call s:HGgrep(<f-args>)")

" }}}1
" mercenary://root_dir//grep {{{1
function! s:method_handlers.grep(regex,path) dict abort
  call s:DoGrep( s:repo(), a:regex, a:path )

  setlocal nomodified nomodifiable readonly filetype=grep
endfunction

function! s:DoGrep( repo, regex, path ) 
  let args = ['grep', '-nuadfq', '--all', a:regex, a:path]
  let hg_grep_applied_cmd = call( a:repo.hg_command, args, a:repo )
  let temppath = resolve(tempname())
  let outfile = temppath . '.grep'
  let errfile = temppath . '.err'

  silent! execute '!'.hg_grep_applied_cmd.' > '.outfile.' 2> '.errfile
  silent! execute 'read '.outfile
endfunction

function! s:GrepSyntax() abort
  " BUILD:249:20:asuffiel:Tue Jun 04 11:28:55 2013 +0100:    output_uid=$(stat -f '%u' "$output_dir")
  let b:current_syntax = 'grep'
  syn match GREPpath '\v^\a{-}:' nextgroup=GREPcs
  syn match GREPcs '\v\d{-}:' nextgroup=GREPlnum
  syn match GREPlnum '\v\d{-}:' nextgroup=GREPauthor
  syn match GREPauthor '\v\a{-}:' nextgroup=GREPdate
  syn match GREPdate '\v.+\d{-}:' nextgroup=GREPline
  syn match GREPline '\v\S{-}$' 

  hi def link GREPpath Identifier
  hi def link GREPcs Operator
  hi def link GREPlnum Function
  hi def link GREPauthor Comment
  hi def link GREPdate Keyword
  hi def link GREPline Variable
endfunction


function! s:GrepCommand(cmd) abort
  call s:DoGrep( s:buffer().repo() )
  redraw!
endfunction

augroup mercurial_grep
  autocmd BufReadPost *.grep setfiletype grep
  autocmd Syntax grep call s:GrepSyntax()
augroup END

" }}}1
" :HGqseries {{{1
function! s:HGqueue() abort
  let winnr = s:buffer_cache.vsplit()
  call s:buffer_cache.edit( winnr, s:gen_mercenary_path('qseries' ) )
endfunction

call s:add_command("-nargs=0 HGqueue call s:HGqueue()")

" }}}1
" mercenary://root_dir//qapplied {{{1
function! s:method_handlers.qseries() dict abort
  call s:DoMq( s:repo() )

  setlocal nomodified nomodifiable readonly filetype=qseries
  nnoremap <buffer> <silent> l :<C-U>exe <SID>Mq('')<CR>
  nnoremap <buffer> <silent> r :<C-U>exe <SID>Mqrefresh()<CR>
  nnoremap <buffer> <silent> + :<C-U>exe <SID>MqCommand('qpush')<CR>
  nnoremap <buffer> <silent> - :<C-U>exe <SID>MqCommand('qpop')<CR>
  " nnoremap <buffer> <silent> <cr> :<C-U>exe <SID>MqCommand('qgoto')<CR>
  " nnoremap <script> <silent> <buffer> ?   :call <sid>buffer().dump()<CR>
  " nnoremap <script> <silent> <buffer> q   :call <sid>MercClose()<CR>
  " cabbrev  <script> <silent> <buffer> q    call <sid>MercClose()
  " cabbrev  <script> <silent> <buffer> quit call <sid>MercClose()
endfunction

function! s:DoMq( repo ) 
  setlocal modifiable noreadonly
  let args = ['qunapplied', '-sv']
  let hg_mq_series_cmd = call( a:repo.hg_command, args, a:repo )

  let temppath = resolve(tempname())
  let outfile = temppath . '.qseries'
  let errfile = temppath . '.err'


  g/^/d
  call append( line('$'), '== Unapplied MQ patches ==')
  call append( line('$'), "" )
  silent! execute '!'.hg_mq_series_cmd.' > '.outfile.' 2> '.errfile
  silent! execute 'read '.outfile
  g/^/m0   "reverse  the patches
  call append( line('$'), "" )
  call append( line('$'), '== Applied MQ patches ==')
  call append( line('$'), "" )
  let tipos = line('$')
  call cursor( tipos, 0 )

  let args = ['glog', '--template', 'cs:      [{rev}:{node|short}] {tags}\nsummary: {desc|firstline|fill68|tabindent|tabindent}\n\n', '-rqtip:qbase' ]
  let hg_glog_cmd = call(a:repo.hg_command, args, a:repo)
  let outfile = temppath . '.glog'
  let errfile = temppath . '.err'
  silent! execute '!'.hg_glog_cmd.' > '.outfile.' 2> '.errfile
  silent! execute 'read '.outfile
  call cursor( tipos, 0 )
  setlocal nomodified nomodifiable readonly filetype=qseries
endfunction

function! s:MqSyntax() abort
  call s:GlogSyntax()
  let b:current_syntax = 'qseries'
  syn match MQApplied '\v(cs|summary)\:' 
  syn match MQID '\v^\d+' 
  syn region MQUnappliedRegion start=' Unapplied' end=' Applied' transparent
  " syn match MQAppliedName '\v\S+\:' contained containedin=MQAppliedRegion "nextgroup=MQAppliedHeader
  syn match MQUnappliedName '\v\S\+\:' contained containedin=MQUnappliedRegion "nextgroup=MQUnappliedHeader
  " syn match MQAppliedHeader '\:\zs.*' contained containedin=MQAppliedRegion
  syn match MQUnappliedHeader '\:\zs.*' contained containedin=MQUnappliedRegion
  hi def link MQApplied Function
  hi def link MQID Identifier
  hi def link MQUnappliedName Operator
  hi def link MQUnappliedHeader Keyword
  " hi def link MQAppliedName Function
  " hi def link MQAppliedHeader Comment
endfunction

" Process the selection of a queue
function! s:Mqrefresh() abort
  let current = line('.')
  call s:DoMq( s:buffer().repo() )
  setlocal nomodified nomodifiable readonly filetype=qseries
  redraw!
  execute current
endfunction

function! s:MqCommand(cmd) abort
  " echom hg_mq
  let hg_cmd = s:buffer().base().repo().hg_command(a:cmd)
  let temppath = resolve(tempname())
  let outfile = temppath . '.mqcmd'
  let errfile = temppath . '.err'

  " Write the blame output to a .mercenaryblame file in a temp folder somewhere
  silent! execute '!' . hg_cmd . ' > ' . outfile . ' 2> ' . errfile
  if v:shell_error 
    call s:warn( "failed to " . a:cmd . ": " . readfile(errfile) )
  else
    call s:Mqrefresh()
  endif
  redraw!
endfunction

augroup mercurial_queue
  autocmd BufReadPost *.qseries setfiletype qseries
  autocmd Syntax qseries call s:MqSyntax()
augroup END

" }}}1
" :HGdiff {{{1
function! s:DoDiff( winnr, buffer, ... ) abort
    let rev = a:0 ? a:1 : 'p1()'
    let merc_p1_path = s:gen_mercenary_path('cat', rev, a:buffer.relpath())

    execute a:winnr ."wincmd w"
    diffthis

    let winnr = s:buffer_cache.vsplit()
    call s:buffer_cache.edit( winnr, merc_p1_path )
    execute winnr ."wincmd w"
    diffthis
    wincmd p

    " if we asked for a parent, then consider merge changeset's or other
    " second parent
    if rev =~ '^p1(.*)$'
      let l:parent2 = substitute( rev, '1', '2', 'i' )
      let hg_parent_check_log_cmd = a:buffer.base().repo().hg_command('log', '--rev', l:parent2)
      let hg_parent_check_log = system(hg_parent_check_log_cmd) 

      if hg_parent_check_log != ""
        let merc_p2_path = s:gen_mercenary_path('cat', l:parent2, a:buffer.relpath())
        let winnr = s:buffer_cache.vsplit("rightbelow")
        call s:buffer_cache.edit( winnr, merc_p2_path )
        execute winnr ."wincmd w"
        diffthis
        wincmd p
      endif
    endif

endfunction

function! s:Diff(...) abort
  " trick to pass vargs onto another function
  let arglist = copy( a:000 )
  call extend( arglist, [ winnr(), s:buffer() ], 0 )
  call call( "s:DoDiff", arglist )
endfunction

call s:add_command("-nargs=? HGdiff call s:Diff(<f-args>)")

" }}}1
