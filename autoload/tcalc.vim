" tcalc.vim
" @Author:      Thomas Link (mailto:micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2007-10-08.
" @Revision:    0.0.466

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


ruby <<EOR
module TCalc
    @stack  = []
    @format = '%p'
    @cmds   = [
        'ls',
        'yank', 'y',
        'let',
        'hex', 'HEX', 'oct', 'bin', 'dec', 'print', 'inspect', 'float', 'format',
        'dup', 'd',
        'copy', 'c',
        'pop', 'p', '.',
        'del', 'delete',
        'rot', 'r',
        'swap', 's',
        'group', 'g',
        'ungroup', 'u',
        'if', 'ifelse',
        'recapture',
        'clear',
        'debug',
        'scope_begin', 'scope_end',
        '#',
    ]
    @words   = {}
    @word_rx = '[[:alpha:]_]+'
    @debug   = false
    @scope   = []

    module_function
    def tokenize(string)
        string.scan(/("(\\"|[^"])*?"|\S+)+/).map {|a,b| a}
    end

    @iqueue  = tokenize(VIM::evaluate("g:tcalc_initialize"))

    def repl
        loop do
            if @iqueue.empty?
                dstack = format(@stack).join("\n")
                VIM::evaluate(%{s:DisplayStack(split(#{dstack.inspect}, '\n'))})
                cmdi = VIM::evaluate("input('> ', '', 'customlist,tcalc#Complete')")
                break if cmdi.empty?
                @iqueue = tokenize(cmdi)
            end
            while !@iqueue.empty?
                begin
                    cmd = @iqueue.shift
                    puts cmd if @debug
                    if !cmd.kind_of?(String)
                        @stack << cmd
                    elsif cmd == '('
                        idx   = nil
                        depth = 0
                        for i in 0..(@iqueue.size - 1)
                            case @iqueue[i]
                            when '('
                                depth += 1
                            when ')'
                                if depth == 0
                                    idx = i
                                    break
                                else
                                    depth -= 1
                                end
                            end
                        end
                        if idx
                            @stack << @iqueue[0..idx - 1]
                            @iqueue[0..idx] = nil
                        else
                            VIM::command("echoerr 'Unmatched ('")
                        end
                    elsif cmd =~ /^-?\d/
                        @stack << eval(cmd).to_f
                    elsif cmd =~ /^"(.*)"$/
                        @stack << eval(cmd)
                    elsif cmd =~ /^'(.*)$/
                        @stack << $1
                    elsif cmd =~ /^#(\d+)?$/
                        n = 1 + ($1 || 1).to_i
                        val = @stack.delete_at(-n)
                        @stack << val if val
                    elsif cmd =~ /^(#@word_rx)=$/
                        @words[$1] = [@stack.pop]
                    elsif cmd =~ /^:(#@word_rx)$/
                        idx = @iqueue.index(';')
                        # @words[$1] = [%{"#{$1} scope_begin}, *@iqueue[0 .. idx - 1]] << 'scope_end'
                        @words[$1] = @iqueue[0 .. idx - 1]
                        @iqueue[0 .. idx] = nil
                    else
                        cmdm = /^(.|#?#@word_rx)(#|\d+)?(,(.+))?$/.match(cmd)
                        cmda = cmdm[1]
                        cmdn = cmdm[2]
                        cmdx = cmdm[4]
                        if cmda =~ /^#(#@word_rx)$/
                            cmda = '#'
                            cmdw = $1
                        else
                            cmdw = nil
                        end
                        case cmdn
                        when '#'
                            cmdn = @stack.pop.to_i
                        when nil, ''
                            cmdn = 1
                        else
                            cmdn = cmdn.to_i
                        end
                        # p "DBG", cmda, cmdn, cmdx
                        cmdn.times do
                            if @cmds.include?(cmda)
                                case cmda
                                when 'debug'
                                    @debug = cmdn.to_i != 0
                                when 'ls'
                                    for key, val in @words
                                        puts "#{key}: #{val.join(' ')}"
                                    end
                                    VIM::evaluate("input('-- Press ENTER --')")
                                when 'yank', 'y'
                                    args = format(@stack[-cmdn .. -1])
                                    args = args.join("\n")
                                    VIM::command("let @#{cmdx || '*'} = #{args.inspect}")
                                    break
                                when 'let'
                                    @words[cmdx] = [@stack.pop]
                                when 'unlet', 'rm'
                                    @words.delete(cmdx)
                                when 'scope_begin'
                                    @scope << @stack.pop
                                when 'scope_end'
                                    @scope.pop
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
                                when 'print', 'inspect'
                                    @format = '%p'
                                when 'float'
                                    @format = '%f'
                                when 'format'
                                    @format = cmdx
                                when 'copy', 'c'
                                    @stack << @stack[-cmdn - 1]
                                    break
                                when 'dup', 'd'
                                    @stack << @stack[-1] unless @stack.empty?
                                when 'del', 'delete'
                                    @stack.delete_at(-cmdn - 1)
                                    break
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
                                when 'g', 'group'
                                    acc = []
                                    cmdn.times {acc << @stack.pop}
                                    @stack << acc.reverse
                                    break
                                when 'u', 'ungroup'
                                    # @iqueue.unshift(*@stack.pop)
                                    @stack.concat(@stack.pop)
                                when 'recapture'
                                    block = @stack.pop
                                    cmdn.times {@iqueue.unshift(*block)}
                                    break
                                when 'clear'
                                    @stack = []
                                    break
                                when 'if'
                                    test, ifblock = @stack[-2..-1]
                                    @stack[-2..-1] = nil
                                    if test
                                        @iqueue.unshift(*ifblock)
                                    end
                                when 'ifelse'
                                    test, ifblock, elseblock= @stack[-3..-1]
                                    @stack[-3..-1] = nil
                                    if test
                                        @iqueue.unshift(*ifblock)
                                    else
                                        @iqueue.unshift(*elseblock)
                                    end
                                when '#'
                                    if cmdw
                                        item = @words[cmdw][0]
                                    else
                                        item = @stack.delete_at(-cmdn - 1)
                                    end
                                    argn = item.method(cmdx).arity
                                    args = get_args(1, argn)
                                    val  = 
                                    if cmdw
                                        @words[cmdw].map! {|item| item.send(cmdx, *args)}
                                    else
                                        @stack << item.send(cmdx, *args)
                                    end
                                end
                            elsif Float.instance_methods.include?(cmda)
                                argn = Float.instance_method(cmda).arity
                                args = get_args(0, argn)
                                @stack += [args[0].send(cmda, *args[1..-1])].flatten
                            elsif Float.constants.include?(cmda)
                                @stack << Float.const_get(cmda)
                            elsif Math.constants.include?(cmda)
                                @stack << Math.const_get(cmda)
                            elsif Math.methods.include?(cmda)
                                argn = Math.method(cmda).arity
                                args = get_args(1, argn)
                                @stack += [Math.send(cmda, *args)].flatten
                            elsif @words.has_key?(cmda)
                                @iqueue.unshift(*@words[cmda])
                            elsif VIM::evaluate("exists('g:tcalc_shortcut_#{cmda}')") == '1'
                                sc = tokenize(VIM::evaluate("g:tcalc_shortcut_#{cmda}"))
                                @iqueue.unshift(*sc)
                            end
                        end
                    end
                rescue Exception => e
                    VIM::command("echoerr #{e.to_s.inspect}")
                end
            end
        end
    end

    def get_args(from, to)
        args = []
        for i in from..to
            args << @stack.pop unless @stack.empty?
        end
        args.reverse
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
        ids = Float.instance_methods | Float.constants | Math.methods | Math.constants | @words.keys | @cmds
        ids.delete_if {|e| e !~ alx}
    end
end


EOR
