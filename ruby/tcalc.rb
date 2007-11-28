#!/usr/bin/env ruby
# tcalc.rb
# @Last Change: 2007-11-28.
# Author::      Thomas Link (micathom AT gmail com)
# License::     GPL (see http://www.gnu.org/licenses/gpl.txt)
# Created::     2007-10-23.
#
# TODO:
# - refactor stack & iqueue handling in order to enable local stacks
# - make the stack, the iqueue, and the dictionary first class objects
# (so that they can be saved in variables, be passed around as arguments 
# etc.)
# - reimplement the whole stuff in java or maybe d? ... or whatever
# - call this interpreter "whatever" :)


require 'matrix'
require 'mathn'
require 'optparse'
# require 'pp'


module TCalc
    CONFIG_DIR = ENV['TCALC_HOME'] || File.join(ENV['HOME'], '.tcalc')

    module_function
    def lib_filename(filename)
        File.join(CONFIG_DIR, filename) if File.directory?(CONFIG_DIR)
    end
end


class TCalc::Base
    FLOAT_PERIOD = (1.1).to_s[1..1]
    attr_accessor :eval_and_exit

    def initialize(&block)
        @cmds = [
            'ls',
            'yank', 'y',
            'define', 'let', 'rm', 'unlet',
            'hex', 'HEX', 'oct', 'bin', 'dec', 'print', 'inspect', 'float', 'format',
            'dup', 'd',
            'copy', 'c',
            'pop', 'p', '.',
            'del', 'delete',
            'rot', 'r',
            'swap', 's',
            'stack_empty?', 'stack_size',
            'iqueue_empty?', 'iqueue_size',
            'Array', 'group', 'g',
            'ungroup', 'u',
            'Sequence', 'seq',
            'map', 'mmap',
            'plot',
            'if', 'ifelse',
            'recapture', 'do',
            'clear',
            'debug',
            'begin', 'end',
            'Rational', 'Complex', 'Integer', 'Matrix',
            'at',
            'args', 'assert', 'validate',
            'source', 'require',
            'history',
            'p',
            # 'puts', 'pp',
            '#',
        ]
        @ymarks = ['+', '*', 'x', '.', '#', ':', '°', '^', '@', '$', 'o', '"']
        reset_words
        reset
        @format  = '%p'
        @word_rx = '[[:alpha:]_]+'
        @debug   = $DEBUG
        @history = []
        @history_size  = 30
        @eval_and_exit = false
        @numclasses    = [Float, Complex, Rational, Integer, Matrix, Vector]
        setup
        instance_eval &block if block
    end


    def setup
        iqueue_reset initial_iqueue
    end



    def tokenize(string)
        string.scan(/("(\\"|[^"])*?"|\S+)+/).map {|a,b| a}
    end


    def repl(initial=[])
        (iqueue).concat(initial) unless initial.empty?
        loop do
            if iqueue_empty?
                if @eval_and_exit
                    dump_stack
                    return
                else
                    display_stack
                    cmdi = read_input
                    break if quit?(cmdi)
                    @history.unshift(cmdi)
                    @history[@history_size..-1] = nil if @history.size > @history_size
                    iqueue_reset tokenize(cmdi)
                end
            end
            while !iqueue_empty?
                begin
                    cmd = iqueue_shift
                    puts cmd if @debug
                    if !cmd.kind_of?(String)
                        stack_push cmd
                    elsif cmd == '(' or cmd == '(('
                        body  = []
                        case cmd
                        when '(('
                            depth = 1
                            body << 'begin' << '('
                        else
                            depth = 0
                        end
                        while !iqueue_empty?
                            elt = iqueue_shift
                            case elt
                            when '('
                                depth += 1
                                body << elt
                            when ')'
                                if depth == 0
                                    if cmd == '(('
                                        body << 'end'
                                    end
                                    break
                                else
                                    if depth == 1
                                        if body.last == '('
                                            body.pop
                                        else
                                            body << ')' << 'args'
                                        end
                                    end
                                    depth -= 1
                                end
                            else
                                body << elt
                            end
                        end
                        if depth == 0
                            stack_push body
                        else
                            echo_error 'Unmatched ('
                        end
                    elsif cmd == '['
                        stack_push '['
                    elsif cmd == ']'
                        start = stack.rindex('[')
                        if start
                            arr = stack_get(start + 1 .. -1)
                            stack_set start .. -1, nil
                            stack_push arr
                        else
                            echo_error 'Unmatched ]'
                        end
                    elsif cmd =~ /^-?\d/
                        stack_push eval(cmd).to_f
                    elsif cmd =~ /^"(.*)"$/
                        stack_push eval(cmd)
                    elsif cmd =~ /^'(.*)$/
                        stack_push $1
                    elsif cmd =~ /^#(\d+)?$/
                        n = 1 + ($1 || 1).to_i
                        val = (stack).delete_at(-n)
                        stack_push val if val
                    elsif cmd =~ /^(#@word_rx)=$/
                        set_word($1, stack_pop)
                    # elsif cmd =~ /^:(\(.+?\))?(#@word_rx)$/
                    elsif cmd =~ /^:(#@word_rx)$/
                        idx = iqueue.index(';')
                        def_lambda($1, iqueue_get(0 .. idx - 1))
                        iqueue_set 0 .. idx, nil
                    elsif cmd == '->'
                        set_word iqueue_shift, stack_pop
                    elsif cmd == '/*'
                        idx = iqueue.index('*/')
                        iqueue_set 0 .. idx, nil
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
                            cmdn = stack_pop.to_i
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
                                        iqueue_unshift(*tokenize(@history[n.to_i]))
                                    end

                                when 'source'
                                    filename = stack_pop
                                    if check_this(String, filename, cmda)
                                        unless File.exist?(filename)
                                            filename = lib_filename(filename)
                                        end
                                        if File.exist?(filename)
                                            contents = File.read(filename)
                                            iqueue_unshift(*tokenize(contents))
                                        else
                                            echo_error 'source: File does not exist'
                                        end
                                    end
                                    break

                                when 'require'
                                    require stack_pop if check_this(String, stack_get(-1), cmda)
                                    #
                                # when 'puts'
                                #     puts stack_pop
                                #     press_enter

                                when 'p'
                                    p stack_pop
                                    press_enter

                                # when 'pp'
                                #     pp stack_pop
                                #     press_enter

                                when 'debug'
                                    @debug = cmdn.to_i != 0

                                when 'assert'
                                    assertion = stack_pop
                                    unless check_assertion(assertion)
                                        iqueue_reset
                                        break
                                    end

                                when 'args'
                                    assertion = stack_pop
                                    ok, names = descriptive_assertion(assertion)
                                    names.compact.each do |name|
                                        # p "DBG", name
                                        set_word(name, stack_pop)
                                    end
                                    unless ok
                                        iqueue_reset
                                        break
                                    end

                                when 'validate'
                                    assertion = stack_pop
                                    stack_push check_assertion(assertion, true)
                                    break

                                when 'ls'
                                    wd = words
                                    ls = wd.keys.sort.map do |key|
                                        next if key =~ /^_/
                                        val = wd[key]
                                        case val
                                        when Array
                                            val = val.join(' ')
                                        else
                                            val = val.inspect
                                        end
                                        "#{key}: #{val}"
                                    end
                                    print_array(ls.compact, true, false)
                                    press_enter

                                when 'yank', 'y'
                                    args = format(stack_get(-cmdn .. -1))
                                    args = args.join("\n")
                                    export(cmdx, args)
                                    break

                                when 'define'
                                    name = stack_pop
                                    body = stack_pop
                                    def_lambda(name, body)

                                when 'let'
                                    set_word(cmdx, stack_pop)

                                when 'unlet', 'rm'
                                    words.delete(cmdx)

                                when 'begin'
                                    scope_begin

                                when 'end'
                                    scope_end

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
                                    stack_push stack_get(-cmdn - 1)
                                    break

                                when 'dup', 'd'
                                    stack_push stack_get(-1) unless stack_empty?

                                when 'del', 'delete'
                                    (stack).delete_at(-cmdn - 1)
                                    break

                                when 'stack_empty?'
                                    stack_push stack_empty?
                                    break

                                when 'stack_size'
                                    stack_push stack.size
                                    break

                                when 'iqueue_empty?'
                                    stack_push iqueue_empty?
                                    break

                                when 'iqueue_size'
                                    stack_push iqueue.size
                                    break

                                when 'pop', 'p', '.'
                                    stack_pop

                                when 'rot', 'r'
                                    n = cmdn + 1
                                    (stack).insert(-n, stack_pop)
                                    break

                                when 'swap', 's'
                                    n = cmdn + 1
                                    val = stack_get(-n .. -1).reverse
                                    stack_set -n .. -1, val
                                    break

                                when 'g', 'group', 'Array'
                                    acc  = []
                                    rows = cmdn
                                    rows.times {acc << stack_pop}
                                    stack_push acc.reverse
                                    break

                                when 'u', 'ungroup'
                                    # iqueue_unshift(*stack_pop)
                                    (stack).concat(stack_pop)

                                when 'recapture', 'do'
                                    block = stack_pop
                                    cmdn.times {iqueue_unshift(*block)} if check_this(Array, block, cmda)
                                    break

                                when 'clear'
                                    stack_reset
                                    break

                                when 'if'
                                    test, ifblock = stack_get(-2..-1)
                                    stack_set -2..-1, nil
                                    if test
                                        iqueue_unshift(*ifblock)
                                    end

                                when 'ifelse'
                                    test, ifblock, elseblock= stack_get(-3..-1)
                                    stack_set -3..-1, nil
                                    if test
                                        iqueue_unshift(*ifblock)
                                    else
                                        iqueue_unshift(*elseblock)
                                    end

                                when 'at'
                                    index = stack_pop
                                    item  = stack_pop
                                    case index
                                    when Array
                                        stack_push item[*index]
                                    else
                                        stack_push item[index]
                                    end

                                when 'map'
                                    fun  = stack_pop
                                    seq  = stack_pop
                                    iseq = seq.map do |xval|
                                        [xval, fun, '#1,<<']
                                    end
                                    stack_push []
                                    iqueue_unshift *(iseq.flatten)

                                when 'mmap'
                                    fun  = stack_pop
                                    seq  = stack_pop
                                    iseq = seq.map do |xval|
                                        ['[', xval, xval, fun, ']', '#1,<<']
                                    end
                                    stack_push []
                                    iqueue_unshift *(iseq.flatten)

                                when 'plot'
                                    xdim = stack_pop
                                    ydim = stack_pop
                                    vals = stack_pop
                                    plot(ydim, xdim, vals, cmdx)

                                when 'seq', 'Sequence'
                                    step = stack_pop
                                    top  = stack_pop
                                    idx  = stack_pop
                                    acc  = []
                                    while idx <= top
                                        acc << idx
                                        idx += step
                                    end
                                    stack_push acc

                                when 'Integer'
                                    stack_push stack_pop.to_i

                                when 'Rational'
                                    denominator = stack_pop
                                    numerator   = stack_pop
                                    stack_push Rational(numerator.to_i, denominator.to_i)

                                when 'Complex'
                                    imaginary = stack_pop
                                    real      = stack_pop
                                    if check_this(Numeric, imaginary, cmda) and check_this(Numeric, real, cmda)
                                        stack_push Complex(real, imaginary)
                                    end

                                when 'Matrix'
                                    if check_this(Array, stack_get(-1), cmda)
                                        stack_push Matrix[ *stack_pop ]
                                    end

                                when '#'
                                    if cmdw
                                        item = get_word(cmdw)
                                    else
                                        item = (stack).delete_at(-cmdn - 1)
                                    end
                                    argn = item.method(cmdx).arity
                                    args = get_args(1, argn)
                                    val  = item.send(cmdx, *args)
                                    stack_push val
                                end

                            elsif words.has_key?(cmda)
                                # p "DBG t2"
                                # p "DBG word"
                                if cmda =~ /^__.*?__$/
                                    iqueue_unshift(get_word(cmda).dup)
                                else
                                    iqueue_unshift(*get_word(cmda))
                                end

                            else
                                # p "DBG t4"
                                catch(:continue) do
                                    @numclasses.each do |c|
                                        if c.instance_methods.include?(cmda)
                                            # p "DBG #{c}"
                                            argn = c.instance_method(cmda).arity
                                            args = get_args(0, argn)
                                            # p "DBG", cmda, argn, args
                                            val = [args[0].send(cmda, *args[1 .. -1])].flatten
                                            # p "DBG", val
                                            (stack).concat(val)
                                            throw :continue
                                        end
                                    end
                                    [Math, *@numclasses].each do |c|
                                        if c.constants.include?(cmda)
                                            # p "DBG #{c} constant"
                                             stack_push c.const_get(cmda)
                                             throw :continue
                                        end
                                    end
                                    if Math.methods.include?(cmda)
                                        # p "DBG t3", cmda
                                        # p "DBG math"
                                        argn = Math.method(cmda).arity
                                        args = get_args(1, argn)
                                        (stack).concat([Math.send(cmda, *args)].flatten)
                                        throw :continue
                                    end
                                    completion = complete_command(cmda, cmdn, cmdx)
                                    if completion
                                        iqueue_unshift(completion)
                                    else
                                        echo_error "Unknown or ambiguous command: #{cmda}"
                                    end
                                end
                            end
                        end
                    end
                rescue Exception => e
                    if @debug
                        raise e
                    elsif @eval_and_exit
                        echo_error '%s: %s' % [e.class, e.to_s]
                        exit 5
                    else
                        echo_error e.to_s.inspect
                    end
                end
            end
        end
        cleanup
    end


    def quit?(input)
        input.nil? || input =~ /^(bye|exit|quit|)$/
    end


    def cleanup
        puts 'Bye!'
    end


    def reset
        stack_reset
        iqueue_reset
        self
    end


    def reset_words
        @words_stack = [{'__WORDS__' => nil}]
    end


    def words
        @words_stack[0]
    end


    def get_word(word)
        case word
        when '__WORDS__'
            rv = words.dup
            rv.delete_if {|k,v| k =~ /^__.*?__$/}
            [ rv ]
        else
            (words)[word]
        end
    end


    def set_word(word, value)
        # p "DBG set_word", word, value, words
        case word
        when '__WORDS__'
            w = value.dup
            @words_stack[0].each {|k,v| w[k] = v if k =~ /^__.*?__$/}
            @words_stack[0] = w
        else
            (words)[word] = value
        end
    end


    def def_lambda(word, body)
        word_def = ['begin', *body] << 'end'
        set_word(word, word_def)
    end


    def stack
        get_word '__STACK__'
    end


    def stack_reset(val=[])
        set_word '__STACK__', val
    end


    def stack_get(pos)
        (stack)[pos]
    end


    def stack_set(pos, arg)
        (stack)[pos] = arg
    end


    def stack_push(arg)
        (stack) << arg
    end


    def stack_pop
        (stack).pop
    end


    def stack_empty?
        (stack).empty?
    end


    def iqueue
        get_word '__IQUEUE__'
    end


    def iqueue_reset(val=[])
        set_word '__IQUEUE__', val
    end


    def iqueue_get(pos)
        (iqueue)[pos]
    end


    def iqueue_set(pos, arg)
        (iqueue)[pos] = arg
    end


    def iqueue_unshift(*args)
        (iqueue).unshift(*args)
    end


    def iqueue_shift
        (iqueue).shift
    end


    def iqueue_empty?
        (iqueue).empty?
    end


    # This is probably the most expensive way to do this. Also, for 
    # objects like Array, Hash, this is likely to yield unexpected 
    # results.
    # This needs to be changed. For the moment it has to suffice though.
    def scope_begin
        @words_stack.unshift(words.dup)
    end


    def scope_end
        if @words_stack.size > 1
            @words_stack.shift
        else
            echo_error 'Scope error: end without begin'
        end
    end


    def plot(ydim, xdim, yvals, register)
        yyvals = yvals.map {|x,y| y}
        ymax   = yyvals.max
        ymin   = yyvals.min
        yrange = ymax - ymin
        xmin   = 0
        xmax   = 0
        xmarks = [nil] * xdim
        yscale = (ydim - 1) / yrange
        xscale = (xdim - 1) / (yvals.size - 1)
        canvas = Array.new(ydim) { ' ' * xdim }

        yvals.each_with_index do |xy, i|
            xpos = [[0, (i * xscale).round].max, xdim - 1].min
            xval, *yvals = xy
            if xval < xmin
                xmin = xval
            elsif xval > xmax
                xmax = xval
            end
            xmarks[xpos] = xval
            ylast = yvals.size - 1
            yvals.reverse.each_with_index do |y, yi|
                ypos = [ydim - 1, [0.0, (y - ymin) * yscale].max].min.round
                if yi == ylast
                    mark = xval.to_s.split(/[,.]/, 2)
                    mark = mark[1][0..0]
                else
                    mark = @ymarks[yi % @ymarks.size]
                end
                canvas[ypos][xpos] = mark
            end
        end

        ydiml = [ymin.round.to_s.size, ymax.round.to_s.size].max + 4
        ydim.to_i.times do |i|
            canvas[i].insert(0, "% #{ydiml}.2f: " % (ymin + i / yscale))
        end

        # canvas.unshift ''
        # xdiml = [('%.1f' % xmin).size, ('%.1f' % xmax).size].max
        xdiml = [xmin.to_i.abs.to_s.size, xmax.to_i.abs.to_s.size].max
        xlast = nil
        (0..xdiml).each do |i|
            row  = ' ' * xdim 
            (xdim.to_i).times do |j|
                xval0 = xmarks[j]
                if xval0
                    xvalt = xval0.truncate.abs
                    xsig  = xval0 >= 0 ? '+' : '-'
                    xval  = "#{xsig}%#{xdiml}d" % xvalt
                else
                    xval  = xlast
                end
                xch = xval[i..i]
                if j == 0 or j == xdim - 1
                    row[j] = xch
                elsif xsig == '+' and xlast != xval
                    row[j] = xch
                elsif xsig == '-' and xlast == xval and j > 2 and xvalt != 0
                    row[j - 1] = ' '
                    row[j] = xch
                end
                xlast = xval
            end
            canvas.unshift(' ' * (ydiml - 3) + '.    ' + row)
        end

        case register
        when nil, ''
            print_array(canvas, false, false)
            press_enter
        else
            export(register, canvas.reverse.join("\n"))
        end
    end


    def display_stack
        puts
        puts '--------------------------------'
        dstack = format(stack)
        idx  = dstack.size
        idxl = (idx - 1).to_s.size
        dstack.map! do |line|
            idx -= 1
            "%#{idxl}d: %s" % [idx, line]
        end
        puts dstack.join("\n")
    end


    def dump_stack
        dstack = format(stack)
        puts dstack.join(' ')
    end


    def read_input
        print '> '
        STDIN.gets
    end


    def lib_filename(filename)
        TCalc.lib_filename(filename)
    end


    def initial_iqueue
        init = lib_filename('init.tca')
        if File.readable?(init)
            tokenize(File.read(init))
        else
            []
        end
    end


    def export(register, args)
        echo_error 'Export not supported'
    end


    def get_args(from, to)
        args = []
        for i in from..to
            args << stack_pop unless stack_empty?
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
        ok, names = descriptive_assertion(assertion, quiet)
        return ok
    end


    def descriptive_assertion(assertion, quiet=false)
        names = []
        case assertion
        when Array
            ok = true
            assertion.reverse.each_with_index do |a, i|
                item = stack_get(-1 - i)
                ok1, name = check_item(a, item)
                names << name
                if ok and !ok1
                    ok = ok1
                end
            end
        else
            ok, name = check_item(assertion, (stack).last, quiet)
            names << name if name
        end
        return [ok, names]
    end


    def check_item(expected, observed, quiet=false)
        name = nil
        case expected
        when String
            if expected =~ /^(\w+):(.*)$/
                name = $1
                expected = $2
            end
            o = eval(expected)
        else
            o = expected
        end
        return [o ? check_this(o, observed, nil, quiet) : true, name]
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
        idxl = (arr.size - 1).to_s.size
        if reversed
            idx = -1
            arr.each do |e|
                idx += 1
                puts "%#{idxl}d: %s" % [idx, e]
            end
        else
            idx = arr.size
            arr.reverse.each do |e|
                idx -= 1
                puts "%#{idxl}d: %s" % [idx, e]
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


    def complete_command(cmda, cmdn, cmdx)
        eligible = completion(cmda)
        if eligible.size == 1
            return eligible[0]
        else
            return nil
        end
    end


    def completion(alt)
        alx = Regexp.new("^#{Regexp.escape(alt)}.*")
        ids = @numclasses.map {|klass|klass.instance_methods | klass.constants}
        ids += Numeric.constants | Numeric.instance_methods | Math.methods | Math.constants | words.keys | @cmds
        ids.flatten!
        ids.uniq!
        ids.sort!
        ids.delete_if {|e| e !~ alx}
    end
end



class TCalc::VIM < TCalc::Base
    @tcalc = nil

    class << self
        def get_tcalc
            unless @tcalc
                @tcalc = self.new
            end
            @tcalc
        end

        def reset
            get_tcalc.reset.setup
        end

        def repl(initial_args)
            tcalc = get_tcalc
            args  = tcalc.tokenize(initial_args)
            tcalc.repl(args)
        end

        def evaluate(initial_args)
            tcalc = get_tcalc
            tcalc.eval_and_exit = true
            begin
                repl(initial_args)
            ensure
                tcalc.eval_and_exit = false
            end
        end

        def completion(alt)
            @tcalc.completion(alt)
        end
    end


    def setup
        iqueue_reset (initial_iqueue + tokenize(VIM::evaluate("g:tcalc_initialize")))
    end


    def quit?(input)
        input.empty? or input == "\n"
    end


    def cleanup
    end


    def display_stack
        dstack = format(stack).join("\n")
        VIM::evaluate(%{s:DisplayStack(split(#{dstack.inspect}, '\n'))})
    end


    def dump_stack
        dstack = format(stack)
        VIM::command(%{let @" = #{dstack.join(' ').inspect}})
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


class TCalc::CommandLine < TCalc::Base
    @@readline = false

    class << self
        def use_readline(val)
            @@readline = val
        end
    end


    def setup
        trap('INT') do
            cleanup
            exit 1
        end

        history = lib_filename('history.txt')
        if File.readable?(history)
            @history = eval(File.read(history))
        end

        if @@readline
            Readline.completion_proc = proc do |string|
                completion(string)
            end
            def read_input
                Readline.readline('> ', true)
            end
        end

        super
    end


    def cleanup
        super
        history = lib_filename('history.txt')
        if history
            File.open(history, 'w') do |io|
                io.puts @history.inspect
            end
        end
    end


    def press_enter
        # puts '--8<-----------------'
    end

end


class TCalc::Curses < TCalc::CommandLine
    class << self
        def use_readline(val)
            puts 'Input via readline is not yet supported for the curses frontend.'
            puts 'Patches are welcome.'
            exit 5
        end
    end


    def setup
        super
        require 'curses'
        Curses.init_screen
        if (@has_colors = Curses.has_colors?)
            Curses.start_color
            Curses.init_pair(1, Curses::COLOR_YELLOW, Curses::COLOR_RED);
        end
    end


    def cleanup
        Curses.close_screen
        super
    end


    def display_stack
        Curses.clear
        dstack = format(stack)
        print_array(dstack)
    end


    def print_array(arr, reversed=true, align=true)
        Curses.clear
        y0   = Curses::lines - 3
        x0   = 3 + Curses.cols / 3
        arr  = arr.reverse if reversed
        idxs = (arr.size - 1).to_s.size
        idxf = "%0#{idxs}d:"
        xlim = Curses.cols - idxs
        xlin = xlim - x0
        arr.each_with_index do |e, i|
            Curses.setpos(y0 - i, 0)
            Curses.addstr(idxf % i)
            if align
                period = e.rindex(FLOAT_PERIOD) || e.size
                Curses.setpos(y0 - i, x0 - period)
                Curses.addstr(e[0..xlin])
            else
                Curses.setpos(y0 - i, idxs + 2)
                Curses.addstr(e[0..xlim])
            end
        end
        Curses.setpos(y0 + 1, 0)
        Curses.addstr('-' * Curses.cols)
        Curses.refresh
    end


    def read_input(index=0, string='')
        # Curses.setpos(Curses::lines - 1, 0)
        # Curses.addstr('> ' + string)
        # Curses.getstr
        histidx = -1
        curcol  = string.size
        loop do
            Curses.setpos(Curses::lines - 1, 0)
            Curses.addstr('> ' + string + ' ' * (Curses.cols - curcol - 2))
            Curses.setpos(Curses::lines - 1, curcol + 2)
            char = Curses.getch
            case char
            when 27, 91
            # when Curses::KEY_EXIT, 4, 27
            #     return ''
            when Curses::KEY_EOL, 10
                return string
            when Curses::KEY_BACKSPACE, 8
                if curcol > 0
                    string[curcol - 1 .. curcol - 1] = ''
                    curcol -= 1
                end
            when Curses::KEY_CTRL_D, 4
                if curcol < string.size
                    string[curcol .. curcol] = ''
                end
            when Curses::KEY_DOWN, 66
                if histidx > 0
                    histidx -= 1
                    string = @history[histidx].dup
                else
                    string = ''

                end
                curcol = string.size
            when Curses::KEY_UP, 65
                if histidx < (@history.size - 1)
                    histidx += 1
                    string = @history[histidx].dup
                    curcol = string.size
                end
            when Curses::KEY_LEFT, 68
                curcol -= 1 if curcol > 0
            when Curses::KEY_RIGHT, 67
                curcol += 1 if curcol < string.size
            when Curses::KEY_CTRL_E
                curcol = string.size
            when Curses::KEY_CTRL_A
                curcol = 0
            when Curses::KEY_CTRL_I, 9
                s = string[0..curcol - 1]
                m = /\S+$/.match(s)
                if m
                    c0 = m[0]
                    cc = complete_command(c0, nil, nil)
                    if cc
                        string = [s, cc[c0.size .. -1], string[curcol .. - 1]].join
                        curcol += cc.size - c0.size
                    end
                end
            else
                string.insert(curcol, '%c' % char)
                curcol += 1
            end
        end
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
        if @has_colors
            Curses.attron(Curses.color_pair(1));
            Curses.attron(Curses::A_BOLD);
        end
        Curses.addstr(msg)
        if @has_colors
            Curses.attroff(Curses::A_BOLD);
            Curses.attroff(Curses.color_pair(1));
        end
        Curses.refresh
        sleep 1
    end

end



if __FILE__ == $0
    $tcalculator = TCalc::Curses
    eval_and_exit  = false

    cfg = TCalc.lib_filename('config.rb')
    if File.readable?(cfg)
        load cfg
    end

    opts = OptionParser.new do |opts|
        opts.banner =  'Usage: tcalc [OPTIONS] [INITIAL INPUT]'
        opts.separator ''
        opts.separator 'tcalc is a free software with ABSOLUTELY NO WARRANTY under'
        opts.separator 'the terms of the GNU General Public License version 2 or newer.'
        opts.separator ''

        opts.separator 'General Options:'
        opts.on('-e', '--[no-]eval', 'Eval arguments and return (implies --no-curses)') do |bool|
            eval_and_exit = bool
            $tcalculator = TCalc::Base
        end

        opts.on('--[no-]curses', 'Use curses gui') do |bool|
            if bool
                $tcalculator = TCalc::Curses
            else
                $tcalculator = TCalc::CommandLine
            end
        end

        opts.on('--[no-]readline', 'Use readline') do |bool|
            if bool
                require 'readline'
            end
            $tcalculator.use_readline bool
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
    tcalc = $tcalculator.new do
        @eval_and_exit = eval_and_exit
    end
    tcalc.repl(iqueue)
end


# Local Variables:
# revisionRx: REVISION\s\+=\s\+\'
# End:
