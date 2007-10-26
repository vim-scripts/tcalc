" tcalc.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2007-10-23.
" @Revision:    0.0.511

if &cp || exists("loaded_tcalc_autoload")
    finish
endif
let loaded_tcalc_autoload = 1


function! tcalc#Calculator(full_screen) "{{{3
    if a:full_screen
        edit __TCalc__
    else
        split __TCalc__
    end
    setlocal buftype=nofile
    setlocal bufhidden=hide
    setlocal noswapfile
    setlocal nobuflisted
    setlocal modifiable
    setlocal foldmethod=manual
    setlocal foldcolumn=0
    setlocal filetype=
    ruby TCalc.repl
    call s:CloseDisplay()
    echo
endf


function! s:CloseDisplay() "{{{3
    if winnr('$') == 1
        bdelete!
    else
        wincmd c
    endif
endf


function! s:DisplayStack(stack) "{{{3
    norm! ggdG
    let ilen = len(a:stack)
    let imax = len(ilen)
    let lines = map(range(ilen), 'printf("%0'. imax .'s: %s", ilen - v:val - 1, a:stack[v:val])')
    call append(0, lines)
    norm! Gdd
    let rs = min([g:tcalc_lines, ilen])
    if winnr('$') > 1
        exec 'resize '. rs
    endif
    let top = ilen - g:tcalc_lines
    norm! Gzb
    redraw
endf


function! tcalc#Complete(ArgLead, CmdLine, CursorPos) "{{{3
    ruby <<EOR
    ids = TCalc.completion(VIM::evaluate('a:ArgLead'))
    VIM::command("return split(#{ids.join("\n").inspect}, '\n')")
EOR
endf


exec 'rubyfile '. expand('<sfile>:p:h:h') .'/ruby/tcalc.rb'

