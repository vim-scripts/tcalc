" tcalc.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2007-10-07.
" @Revision:    0.0.206

if &cp || exists("loaded_tcalc_autoload")
    finish
endif
let loaded_tcalc_autoload = 1

let s:stack   = []

function! tcalc#Calculator() "{{{3
    let s:scratch = {'scratch': '__TCalc__'}
    call tlib#scratch#UseScratch(s:scratch)
    while 1
        call s:DisplayStack()
        redraw
        let cmdi = input('> ', '', 'customlist,tcalc#Complete')
        if empty(cmdi)
            call s:CloseDisplay()
            echo
            break
        else
            for cmd in split(cmdi, '\s\+')
                if cmd =~ '^-\?\d'
                    call add(s:stack, cmd)
                else
                    ruby <<EOR
                    stack = VIM::evaluate('s:stack').split("\n")
                    cmd  = VIM::evaluate('cmd')
                    cmdm = /^([^0-9[:space:],]+)(\d+)?(,(.+))?$/.match(cmd)
                    cmda = cmdm[1]
                    cmdn = (cmdm[2] || 1).to_i
                    cmdx = cmdm[4]
                    cmdn.times do
                        if Float.instance_methods.include?(cmda)
                            args = []
                            argn = Float.instance_method(cmda).arity
                            for i in 0..argn
                                args << stack.pop.to_f unless stack.empty?
                            end
                            args.reverse!
                            stack += [args[0].send(cmda, *args[1..-1])].flatten
                        elsif Float.constants.include?(cmda)
                            stack << Float.const_get(cmda)
                        elsif Math.constants.include?(cmda)
                            stack << Math.const_get(cmda)
                        elsif Math.methods.include?(cmda)
                            args = []
                            argn = Math.method(cmda).arity
                            for i in 1..argn
                                args << stack.pop.to_f unless stack.empty?
                            end
                            args.reverse!
                            stack += [Math.send(cmda, *args)].flatten
                        else
                            case cmda
                            when 'copy', 'c', 'yank', 'y'
                                args = stack[-cmdn .. -1].join("\n")
                                VIM::command("let @#{cmdx || '*'} = #{args.inspect}")
                            when 'pop', 'p', '.'
                                stack.pop
                            when 'rot', 'r'
                                val = stack.delete_at(-cmdn - 1)
                                stack << val
                                break
                            when 'swap', 's'
                                val = stack[-cmdn .. -1].reverse
                                stack[-cmdn .. -1] = val
                                break
                            else
                                if VIM::evaluate("exists('g:tcalc_shortcut_#{cmda}')") == '1'
                                    val = stack.pop
                                    fmt = VIM::evaluate("tlib#string#Printf1(g:tcalc_shortcut_#{cmda}, #{val.inspect})")
                                    stack << eval(fmt)
                                end
                            end
                        end
                    end
                    VIM::evaluate(%{s:SetStack(#{stack.join("\n").inspect})})
EOR
                endif
            endfor
        endif
    endwh
endf


function! s:SetStack(string) "{{{3
    let s:stack = split(a:string, '\n')
endf


function! s:DisplayStack() "{{{3
    norm! ggdG
    if g:tcalc_reverse_display
        let stack = reverse(copy(s:stack))
    else
        let stack = s:stack
    endif
    call append(0, stack)
    norm! Gdd
    let rs = min([10, len(stack)])
    if winnr('$') > 1
        exec 'resize '. rs
    endif
    norm! gg
endf


function! s:CloseDisplay() "{{{3
    call tlib#scratch#CloseScratch(s:scratch)
endf


function! tcalc#Complete(ArgLead, CmdLine, CursorPos) "{{{3
    ruby <<EOR
    alt = VIM::evaluate('a:ArgLead')
    alx = Regexp.new("^#{Regexp.escape(alt)}.*")
    ids = Float.instance_methods | Float.constants | Math.methods | Math.constants
    ids.delete_if {|e| e !~ alx}
    # ids.map {|e| alp + e}
    VIM::command("return split(#{ids.join("\n").inspect}, '\n')")
EOR
endf

