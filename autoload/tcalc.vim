" tcalc.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2007-10-08.
" @Revision:    0.0.333

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


ruby <<EOR
module TCalc
    @stack  = []
    @format = '%p'
    @cmds   = [
        'copy', 'c', 'yank', 'y',
        'hex', 'HEX', 'oct', 'bin', 'dec', 'print', 'float', 'format',
        'dup', 'd',
        'pop', 'p', '.',
        'rot', 'r',
        'swap', 's',
        'group', 'g', ')',
        'ungroup', 'u', '(',
        'vars', 'ls',
    ]
    @vars = {}

    module_function
    def repl
        loop do
            dstack = format(@stack).join("\n")
            VIM::evaluate(%{s:DisplayStack(split(#{dstack.inspect}, '\n'))})
            cmdi = VIM::evaluate("input('> ', '', 'customlist,tcalc#Complete')")
            break if cmdi.empty?
            iqueue = cmdi.split(/\s+/)
            while !iqueue.empty?
                begin
                    cmd = iqueue.shift
                    if cmd =~ /^-?\d/
                        @stack << eval(cmd).to_f
                    elsif cmd =~ /^#(\d+)?$/
                        n = 1 + ($1 || 1).to_i
                        val = @stack.delete_at(-n)
                        @stack << val if val
                    elsif cmd =~ /^(\w+)=$/
                        @vars[$1] = @stack.pop
                    else
                        cmdm = /^([^0-9[:space:],]+)(\d+)?(,(.+))?$/.match(cmd)
                        cmda = cmdm[1]
                        cmdn = (cmdm[2] || 1).to_i
                        cmdx = cmdm[4]
                        cmdn.times do
                            if Float.instance_methods.include?(cmda)
                                args = []
                                argn = Float.instance_method(cmda).arity
                                for i in 0..argn
                                    args << @stack.pop unless @stack.empty?
                                end
                                args.reverse!
                                @stack += [args[0].send(cmda, *args[1..-1])].flatten
                            elsif Float.constants.include?(cmda)
                                @stack << Float.const_get(cmda)
                            elsif Math.constants.include?(cmda)
                                @stack << Math.const_get(cmda)
                            elsif Math.methods.include?(cmda)
                                args = []
                                argn = Math.method(cmda).arity
                                for i in 1..argn
                                    args << @stack.pop unless @stack.empty?
                                end
                                args.reverse!
                                @stack += [Math.send(cmda, *args)].flatten
                            elsif @vars.has_key?(cmda)
                                @stack << @vars[cmda]
                            else
                                case cmda
                                when 'vars', 'ls'
                                    for key, val in @vars
                                        puts "#{key}: #{val}"
                                    end
                                    VIM::evaluate("input('-- Press ENTER --')")
                                when 'copy', 'c', 'yank', 'y'
                                    args = format(@stack[-cmdn .. -1])
                                    args = args.join("\n")
                                    VIM::command("let @#{cmdx || '*'} = #{args.inspect}")
                                    break
                                when 'let', '='
                                    @vars[cmdx] = @stack.pop
                                when 'unlet', 'rm'
                                    @vars.delete(cmdx)
                                when 'hex'
                                    @format = '%x'
                                when 'HEX'
                                    @format = '%X'
                                when 'oct'
                                    @format = '%o'
                                when 'bin'
                                    @format = '%016b'
                                when 'dec'
                                    @format = '%d'
                                when 'print'
                                    @format = '%p'
                                when 'float'
                                    @format = '%f'
                                when 'format'
                                    @format = cmdx
                                when 'dup', 'd'
                                    @stack << @stack[-1] unless @stack.empty?
                                when 'pop', 'p', '.'
                                    @stack.pop
                                when 'rot', 'r'
                                    n = cmdn + 1
                                    @stack.insert(-n, @stack.pop)
                                    break
                                when 'swap', 's'
                                    n = cmdn + 1
                                    val = @stack[-n .. -1].reverse
                                    @stack[-n .. -1] = val
                                    break
                                when 'g', 'group', ')'
                                    acc = []
                                    cmdn.times {acc << @stack.pop}
                                    @stack << acc.reverse
                                    break
                                when 'u', 'ungroup', '('
                                    @stack += @stack.pop
                                when 'clear'
                                    @stack = []
                                    break
                                else
                                    if VIM::evaluate("exists('g:tcalc_shortcut_#{cmda}')") == '1'
                                        sc = VIM::evaluate("g:tcalc_shortcut_#{cmda}").split(/\s+/)
                                        iqueue = sc + iqueue
                                    end
                                end
                            end
                        end
                    end
                rescue Exception => e
                    VIM::command("echoerr #{e.to_s.inspect}")
                end
            end
        end
    end

    def format(elt, level=1)
        case elt
        when Array
            elt = elt.map {|e| format(e, level + 1)}
            if level > 1
                '[%s]' % elt.join(', ')
            else
                elt
            end
        else
            sprintf(@format, elt)
        end
    end


    def completion(alt)
        alx = Regexp.new("^#{Regexp.escape(alt)}.*")
        ids = Float.instance_methods | Float.constants | Math.methods | Math.constants | @vars.keys | @cmds
        ids.delete_if {|e| e !~ alx}
    end
end


EOR
