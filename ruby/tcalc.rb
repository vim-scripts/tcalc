#!/usr/bin/env ruby
# tcalc.rb
# @Last Change: 2007-10-28.
# Author::      Thomas Link (micathom AT gmail com)
# License::     GPL (see http://www.gnu.org/licenses/gpl.txt)
# Created::     2007-10-23.


require 'matrix'
require 'mathn'
require 'optparse'
# require 'pp'


module TCalc; end


class TCalc::Base
    def initialize
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
            'recapture', 'do',
            'clear',
            'debug',
            'scope_begin', 'scope_end',
            'Rational', 'Complex', 'Integer', 'Matrix',
            'at',
            'assert', 'validate',
            'source', 'require',
            'history',
            'p',
            # 'puts', 'pp',
            '#',
        ]
        @stack   = []
        @iqueue  = []
        @format  = '%p'
        @help    = {}
        @words   = {}
        @word_rx = '[[:alpha:]_]+'
        @debug   = false
        @scope   = []
        @history = []
        @history_size = 30
        setup
    end


    def setup
    end



    def tokenize(string)
        string.scan(/("(\\"|[^"])*?"|\S+)+/).map {|a,b| a}
    end


    def repl(initial=[])
        @iqueue += initial unless initial.empty?
        loop do
            if @iqueue.empty?
                display_stack
                cmdi = read_input
                break if quit?(cmdi)
                @history.unshift(cmdi)
                @history[@history_size..-1] = nil if @history.size > @history_size
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
                            echo_error 'Unmatched ('
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
                            echo_error 'Unmatched ]'
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
                    elsif cmd == '/*'
                        idx = @iqueue.index('*/')
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
                                when 'history'
                                    print_array(@history, false, false)
                                    n = read_input
                                    if n =~ /^\d+$/
                                        @iqueue.unshift(*tokenize(@history[n.to_i]))
                                    end
                                when 'source'
                                    filename = @stack.pop
                                    if check_this(String, filename, cmda)
                                        unless File.exist?(filename)
                                            filename = lib_filename(filename)
                                        end
                                        if File.exist?(filename)
                                            contents = File.read(filename)
                                            @iqueue.unshift(*tokenize(contents))
                                        else
                                            echo_error 'source: File does not exist'
                                        end
                                    end
                                    break
                                when 'require'
                                    require @stack.pop if check_this(String, @stack[-1], cmda)
                                # when 'puts'
                                #     puts @stack.pop
                                #     press_enter
                                when 'p'
                                    p @stack.pop
                                    press_enter
                                # when 'pp'
                                #     pp @stack.pop
                                #     press_enter
                                when 'debug'
                                    @debug = cmdn.to_i != 0
                                when 'assert'
                                    assertion = @stack.pop
                                    unless check_assertion(assertion)
                                        @iqueue = []
                                        break
                                    end
                                when 'validate'
                                    assertion = @stack.pop
                                    @stack << check_assertion(assertion, true)
                                    break
                                when 'ls'
                                    ls = @words.sort.map do |key, val|
                                        help = @help[key]
                                        "#{key}#{help}: #{val.join(' ')}"
                                    end
                                    print_array(ls, true, false)
                                    press_enter
                                when 'yank', 'y'
                                    args = format(@stack[-cmdn .. -1])
                                    args = args.join("\n")
                                    export(cmdx, args)
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
                                when 'recapture', 'do'
                                    block = @stack.pop
                                    cmdn.times {@iqueue.unshift(*block)} if check_this(Array, block, cmda)
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
                                    if check_this(Numeric, imaginary, cmda) and check_this(Numeric, real, cmda)
                                        @stack << Complex(real, imaginary)
                                    end
                                when 'Matrix'
                                    if check_this(Array, @stack[-1], cmda)
                                        @stack << Matrix[ *@stack.pop ]
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
                    echo_error e.to_s.inspect
                end
            end
        end
        cleanup
    end


    def quit?(input)
        input =~ /^(bye|exit|quit|)$/
    end


    def cleanup
        puts 'Bye!'
    end


    def display_stack
        puts '--------------------------------'
        dstack = format(@stack)
        puts dstack.join("\n")
    end


    def read_input
        print '> '
        STDIN.gets
    end


    def lib_filename(filename)
        File.join(ENV['HOME'], '.tcalc', filename)
    end


    def export(register, args)
        echo_error 'Export not supported'
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


    def check_assertion(assertion, quiet=false)
        case assertion
        when Array
            ok = true
            assertion.reverse.each_with_index do |a, i|
                item = @stack[-1 - i]
                unless check_item(a, item)
                    ok = false
                    break
                end
            end
        else
            ok = check_item(assertion, @stack.last, quiet)
        end
        return ok
    end


    def check_item(expected, observed, quiet=false)
        case expected
        when String
            o = eval(expected)
        else
            o = expected
        end
        return check_this(o, observed, nil, quiet)
    end


    def check_this(expected, observed, prefix=nil, quiet=false)
        case expected
        when Class
            ok = observed.kind_of?(expected)
        else
            ok = observed == expected
        end
        unless quiet
            unless ok
                echo_error "#{prefix || 'validate'}: Expected #{expected.to_s}, got #{observed.inspect}"
            end
        end
        return ok
    end


    def print_array(arr, reversed=true, align=true)
        if reversed
            idx = -1
            arr.each do |e|
                idx += 1
                puts '%d: %s' % [idx, e]
            end
        else
            idx = arr.size
            arr.reverse.each do |e|
                idx -= 1
                puts '%d: %s' % [idx, e]
            end
        end
    end

    
    def press_enter
        puts '-- Press ANY KEY --'
        STDIN.getc
    end


    def echo_error(msg)
        puts msg
        sleep 1
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



class TCalc::VIM < TCalc::Base
    @tcalc = nil

    class << self
        def repl
            unless @tcalc
                @tcalc = self.new
            end
            @tcalc.repl
        end

        def completion(alt)
            @tcalc.completion(alt)
        end
    end


    def setup
        @iqueue = tokenize(VIM::evaluate("g:tcalc_initialize"))
    end


    def quit?(input)
        input.empty? or input == "\n"
    end


    def cleanup
    end


    def display_stack
        dstack = format(@stack).join("\n")
        VIM::evaluate(%{s:DisplayStack(split(#{dstack.inspect}, '\n'))})
    end


    def print_array(arr, reversed=true, align=true)
        VIM::command("echo | redraw")
        super
    end


    def read_input
        VIM::evaluate("input('> ', '', 'customlist,tcalc#Complete')")
    end


    def lib_filename(filename)
        File.join(VIM::evaluate('g:tcalc_dir'), filename)
    end


    def export(register, args)
        VIM::command("let @#{register || '*'} = #{args.inspect}")
    end


    def press_enter
        VIM::command("echohl MoreMsg")
        VIM::command("echo '-- Press ANY KEY --'")
        VIM::command("echohl NONE")
        VIM::evaluate("getchar()")
        VIM::command("echo")
    end


    def echo_error(msg)
        VIM::command("echohl error")
        VIM::command("echom #{msg.inspect}")
        VIM::command("echohl NONE")
        VIM::command("sleep 1")
        # press_enter
    end

end



class TCalc::Curses < TCalc::Base
    def setup
        require 'curses'
        Curses.init_screen
    end


    def cleanup
        Curses.close_screen
    end


    def display_stack
        Curses.clear
        dstack = format(@stack)
        print_array(dstack)
    end


    def print_array(arr, reversed=true, align=true)
        Curses.clear
        y0   = Curses::lines - 3
        x0   = 3 + Curses.cols / 3
        arr  = arr.reverse if reversed
        idxs = arr.size.to_s.size
        idxf = "%0#{idxs}d:"
        arr.each_with_index do |e, i|
            Curses.setpos(y0 - i, 0)
            Curses.addstr(idxf % i)
            if align
                period = e.rindex('.') || e.size
            else
                period = 0
            end
            Curses.setpos(y0 - i, align ? (x0 - period) : (idxs + 2))
            Curses.addstr(e)
        end
        Curses.setpos(y0 + 1, 0)
        Curses.addstr('-' * Curses.cols)
        Curses.refresh
    end


    def read_input(index=0, string='')
        Curses.setpos(Curses::lines - 1, 0)
        Curses.addstr('> ' + string)
        Curses.getstr
    end


    def press_enter
        msg = '-- Press ANY KEY --'
        # Curses.setpos(Curses::lines - 1, Curses::cols - msg.size)
        Curses.setpos(Curses::lines - 1, 0)
        Curses.addstr(msg)
        Curses.getch
    end


    def echo_error(msg)
        Curses.setpos(Curses::lines - 1, 0)
        Curses.addstr(msg)
        sleep 1
    end

end



if __FILE__ == $0
    klass = TCalc::Curses
    opts = OptionParser.new do |opts|
        opts.banner =  'Usage: tcalc [OPTIONS] [INITIAL INPUT]'
        opts.separator ''
        opts.separator 'tcalc is a free software with ABSOLUTELY NO WARRANTY under'
        opts.separator 'the terms of the GNU General Public License version 2 or newer.'
        opts.separator ''

        opts.separator 'General Options:'
        opts.on('--[no-]curses', 'Use curses gui') do |bool|
            if bool
                klass = TCalc::Curses
            else
                klass = TCalc::Base
            end
        end

        opts.separator ''
        opts.separator 'Other Options:'

        opts.on('--debug', 'Show debug messages') do |v|
            $DEBUG   = true
            $VERBOSE = true
        end

        opts.on('-v', '--verbose', 'Run verbosely') do |v|
            $VERBOSE = true
        end

        opts.on_tail('-h', '--help', 'Show this message') do
            puts opts
            exit 1
        end
    end
    iqueue = opts.parse!(ARGV)
    klass.new.repl(iqueue)
end


# Local Variables:
# revisionRx: REVISION\s\+=\s\+\'
# End:
