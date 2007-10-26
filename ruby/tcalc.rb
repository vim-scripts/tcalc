#!/usr/bin/env ruby
# tcalc.rb
# @Last Change: 2007-10-24.
# Author::      Thomas Link (micathom AT gmail com)
# License::     GPL (see http://www.gnu.org/licenses/gpl.txt)
# Created::     2007-10-23.
#
# = Description
# = Usage
# = TODO
# = CHANGES

require 'matrix'
require 'mathn'

module TCalc
    @cmds = [
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
        'Rational', 'Complex', 'Integer', 'Matrix',
        'at',
        '#',
    ]
    @stack   = []
    @format  = '%p'
    @help    = {}
    @words   = {}
    @word_rx = '[[:alpha:]_]+'
    @debug   = false
    @scope   = []


    module_function

    def standalone
        require 'curses'
    end


    def tokenize(string)
        string.scan(/("(\\"|[^"])*?"|\S+)+/).map {|a,b| a}
    end


    @iqueue = tokenize(VIM::evaluate("g:tcalc_initialize"))


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
                    elsif cmd == '['
                        @stack << '['
                    elsif cmd == ']'
                        start = @stack.rindex('[')
                        if start
                            arr = @stack[start + 1 .. -1]
                            @stack[start .. -1] = nil
                            @stack << arr
                        else
                            VIM::command("echoerr 'Unmatched ]'")
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
                    # elsif cmd =~ /^:(\(.+?\))?(#@word_rx)$/
                    elsif cmd =~ /^:()(#@word_rx)$/
                        idx = @iqueue.index(';')
                        # @help[$2]  = $1
                        @words[$2] = @iqueue[0 .. idx - 1]
                        @iqueue[0 .. idx] = nil
                    else
                        cmdm = /^(#?[^#,[:digit:]]*)(#|\d+)?(,(.+))?$/.match(cmd)
                        # p "DBG", cmdm
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
                                # p "DBG t1"
                                case cmda
                                when 'debug'
                                    @debug = cmdn.to_i != 0
                                when 'ls'
                                    for key, val in @words.sort
                                        help = @help[key]
                                        puts "#{key}#{help}: #{val.join(' ')}"
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
                                when 'at'
                                    index = @stack.pop
                                    item  = @stack.pop
                                    case index
                                    when Array
                                        @stack << item[*index]
                                    else
                                        @stack << item[index]
                                    end
                                when 'Integer'
                                    @stack << @stack.pop.to_i
                                when 'Rational'
                                    denominator = @stack.pop
                                    numerator   = @stack.pop
                                    @stack << Rational(numerator.to_i, denominator.to_i)
                                when 'Complex'
                                    imaginary = @stack.pop
                                    real      = @stack.pop
                                    @stack << Complex(real, imaginary)
                                when 'Matrix'
                                    @stack << Matrix[ *@stack.pop ]
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
                            elsif @words.has_key?(cmda)
                                # p "DBG t2"
                                # p "DBG word"
                                @iqueue.unshift(*@words[cmda])
                            else
                                # p "DBG t4"
                                catch(:continue) do
                                    [Float, Complex, Rational, Matrix, Vector].each do |c|
                                        if c.instance_methods.include?(cmda)
                                            # p "DBG #{c}"
                                            argn = c.instance_method(cmda).arity
                                            args = get_args(0, argn)
                                            # p "DBG", cmda, argn, args
                                            val = [args[0].send(cmda, *args[1..-1])].flatten
                                            # p "DBG", val
                                            @stack += val
                                            throw :continue
                                        end
                                    end
                                    [Float, Math, Complex, Rational, Matrix, Vector].each do |c|
                                        if c.constants.include?(cmda)
                                            # p "DBG #{c} constant"
                                             @stack << c.const_get(cmda)
                                             throw :continue
                                        end
                                    end
                                    if Math.methods.include?(cmda)
                                        # p "DBG t3", cmda
                                        # p "DBG math"
                                        argn = Math.method(cmda).arity
                                        args = get_args(1, argn)
                                        @stack += [Math.send(cmda, *args)].flatten
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
        ids = Float.instance_methods | Float.constants | 
            Complex.constants | Complex.methods |
            Rational.constants | Rational.methods |
            Matrix.constants | Matrix.methods |
            Vector.constants | Vector.methods |
            Math.methods | Math.constants | @words.keys | @cmds
        ids.delete_if {|e| e !~ alx}
    end
end



if __FILE__ == $0
end


# Local Variables:
# revisionRx: REVISION\s\+=\s\+\'
# End:
