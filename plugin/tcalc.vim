" tcalc.vim -- A RPN calculator for vim
" @Author:      Thomas Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2007-10-08.
" @Revision:    0.3.93
" GetLatestVimScripts: 2040 1 tcalc.vim

if &cp || exists("loaded_tcalc") || !has('ruby')
    finish
endif
let loaded_tcalc = 3

let s:save_cpo = &cpo
set cpo&vim


if !exists('g:tcalc_shortcut_rev') "{{{2
    let g:tcalc_shortcut_rev = '1.0 swap /'
endif

if !exists('g:tcalc_lines') "{{{2
    let g:tcalc_lines = 10
endif


command! -bang TCalc call tcalc#Calculator(!empty('<bang>'))


let &cpo = s:save_cpo
unlet s:save_cpo

finish


A small ruby-based[*] RPN-calculator.

Command:
    :TCalc[!]
        With !, use as full-screen calculator.

Input:
    - Numbers (anything starting with "-" or a decimal)
    - Methods (Float instance methods[1] or Math module methods[2])
    - Shortcuts (apply the g:tcalc_NAME format string)
    - #N (pull the item at position N to the top)
    - ,NAME (push a variable)
    - Commands
        let, =, VARIABLE=
            Define a variable (e.g. let,VARIABLE), the variable can be 
            referenced by it name.
        rm, unlet
            Remove a variable (e.g. rm,VARIABLE)
        vars, ls
            List variables
        p, pop, .
            Pop/remove item(s)
        d, dup
            Duplicate the top item
        r, rot
            Rotate, push the top item to the back
        s, swap
            Reverse slice
        gN, groupN, )N
            Replace N elements with an array
        u, ungroup, (
            Replace an array with its elements
        clear
            Clear the stack
        y, yank, copy, c
            Copy the top N items to a register (* by default). This 
            command takes a register as optional argument, e.g., "y,e"
    - Enter, escape => exit

Every method/shortcut/command may take a count as optional argument to 
repeat the command n times. E.g. "+3" will sum up the top 3 numbers, 
"y3" will copy the top 3 items in the "*" register.

The calculator has command-line completion enabled. But:
    - This only works if you input single tokens at a time, i.e. 
      "0.5<cr>sin<cr>".
    - Be aware that not every method is useful in the context of this 
      plugin.


[*] Built-in ruby support (:echo has('ruby')) is required.
[1] http://www.ruby-doc.org/core/classes/Float.html
[2] http://www.ruby-doc.org/core/classes/Math.html


CHANGES:
0.1
- Initial release

0.2
- Arguments were not properly reverted: 12 4 / now yields 3.
- The input will be split into tokens, i.e. you can input "1 2 + <cr>" 
or "1<cr>2<cr>+<cr>". (Command-line completions doesn't work properly 
though.)
- The syntax has slightly changed: "CmdCount,Arg", eg, "y3,a"

0.3
- The swap count argument is increased by one (for conformance with the 
rot command).
- Shortcuts are now RPN expression (elements at the stack can be 
referred to by # (= top element) or #N).
- Removed g:tcalc_reverse_display
- Positions on the stack can be referred to by #N.
- rot works the other way round
- d, dup command
- clear command
- print, hex, HEX, oct, dec, bin, float, format commands
- Removed dependency on tlib
- Variables; ls, vars, let, =, rm commands
- Command line completion for variables and commands

