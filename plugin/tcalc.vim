" tcalc.vim -- A RPN calculator for vim
" @Author:      Thomas Link (micathom AT gmail com?subject=[vim])
" @Website:     http://www.vim.org/account/profile.php?user_id=4037
" @License:     GPL (see http://www.gnu.org/licenses/gpl.txt)
" @Created:     2007-10-07.
" @Last Change: 2007-10-07.
" @Revision:    0.1.30
" GetLatestVimScripts: 0 1 tcalc.vim


if &cp || exists("loaded_tcalc") || !has('ruby')
    finish
endif
if !exists('g:loaded_tlib') || g:loaded_tlib < 16
    echoerr 'tlib >= 0.16 is required'
    finish
endif
let loaded_tcalc = 1

let s:save_cpo = &cpo
set cpo&vim


TLet g:tcalc_shortcut_rev = '1.0 / ( %s )'

" Display the stack upside-down.
TLet g:tcalc_reverse_display = 0


command! TCalc call tcalc#Calculator()


let &cpo = s:save_cpo
unlet s:save_cpo

finish


A small ruby-based[*] RPN-calculator.

Input:
    - Numbers (anything starting with "-" or a decimal)
    - Methods (Float instance methods[1] or Math module methods[2])
    - Shortcuts (apply the g:tcalc_NAME format string)
    - Commands
        - p, pop, .
        - r, rot
        - s, swap (actually: reverse slice)
        - y, yank, copy, c
            - This command takes a register as optional argument, e.g., 
            "y e"
    - Enter, escape => exit

Every method/shortcut/command may take a count as optional argument to 
repeat the command n times. E.g. "+3" will sum up the top 3 numbers, 
"y3" will copy the top 3 items in the "*" register.

The calculator has command-line completion enabled. Be aware though that 
not every method is useful in the context of this plugin.


[*] Built-in ruby support (:echo has('ruby')) is required.
[1] http://www.ruby-doc.org/core/classes/Float.html
[2] http://www.ruby-doc.org/core/classes/Math.html

