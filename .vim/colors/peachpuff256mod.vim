" Vim color file
" Maintainer: Illia Danko <idanko@tuta.io>
" Last Change: 2018-06-27
"

" Origin colors are taken from the VIM builtin peachpuff.vim GUI color theme
" and can be used either on the 88- or 256-color compatible xterm. The RGB grid
" is the translation of the Henry So's programmatic approximation colors
" algorithm (see a desert256 theme). Also, some part of the code belongs to
" Martin Baeuml <baeuml@gmail.com>, the author, and the maintainer of the
" summerfruit256 vim color theme.
"
" The original "summerfruit256" color scheme along with the "desert256" are
" available on vim.org

set background=light
if version > 580
    " no guarantees for version 5.8 and below, but this makes it stop
    " complaining
    hi clear
    if exists("syntax_on")
        syntax reset
    endif
endif
let g:colors_name="peachpuff256mod"

if has("gui_running") || &t_Co == 88 || &t_Co == 256
    " functions {{{
    " returns an approximate grey index for the given grey level
    fun <SID>grey_number(x)
        if &t_Co == 88
            if a:x < 23
                return 0
            elseif a:x < 69
                return 1
            elseif a:x < 103
                return 2
            elseif a:x < 127
                return 3
            elseif a:x < 150
                return 4
            elseif a:x < 173
                return 5
            elseif a:x < 196
                return 6
            elseif a:x < 219
                return 7
            elseif a:x < 243
                return 8
            else
                return 9
            endif
        else
            if a:x < 14
                return 0
            else
                let l:n = (a:x - 8) / 10
                let l:m = (a:x - 8) % 10
                if l:m < 5
                    return l:n
                else
                    return l:n + 1
                endif
            endif
        endif
    endfun

    " returns the actual grey level represented by the grey index
    fun <SID>grey_level(n)
        if &t_Co == 88
            if a:n == 0
                return 0
            elseif a:n == 1
                return 46
            elseif a:n == 2
                return 92
            elseif a:n == 3
                return 115
            elseif a:n == 4
                return 139
            elseif a:n == 5
                return 162
            elseif a:n == 6
                return 185
            elseif a:n == 7
                return 208
            elseif a:n == 8
                return 231
            else
                return 255
            endif
        else
            if a:n == 0
                return 0
            else
                return 8 + (a:n * 10)
            endif
        endif
    endfun

    " returns the palette index for the given grey index
    fun <SID>grey_color(n)
        if &t_Co == 88
            if a:n == 0
                return 16
            elseif a:n == 9
                return 79
            else
                return 79 + a:n
            endif
        else
            if a:n == 0
                return 16
            elseif a:n == 25
                return 231
            else
                return 231 + a:n
            endif
        endif
    endfun

    " returns an approximate color index for the given color level
    fun <SID>rgb_number(x)
        if &t_Co == 88
            if a:x < 69
                return 0
            elseif a:x < 172
                return 1
            elseif a:x < 230
                return 2
            else
                return 3
            endif
        else
            if a:x < 75
                return 0
            else
                let l:n = (a:x - 55) / 40
                let l:m = (a:x - 55) % 40
                if l:m < 20
                    return l:n
                else
                    return l:n + 1
                endif
            endif
        endif
    endfun

    " returns the actual color level for the given color index
    fun <SID>rgb_level(n)
        if &t_Co == 88
            if a:n == 0
                return 0
            elseif a:n == 1
                return 139
            elseif a:n == 2
                return 205
            else
                return 255
            endif
        else
            if a:n == 0
                return 0
            else
                return 55 + (a:n * 40)
            endif
        endif
    endfun

    " returns the palette index for the given R/G/B color indices
    fun <SID>rgb_color(x, y, z)
        if &t_Co == 88
            return 16 + (a:x * 16) + (a:y * 4) + a:z
        else
            return 16 + (a:x * 36) + (a:y * 6) + a:z
        endif
    endfun

    " returns the palette index to approximate the given R/G/B color levels
    fun <SID>color(r, g, b)
        " get the closest grey
        let l:gx = <SID>grey_number(a:r)
        let l:gy = <SID>grey_number(a:g)
        let l:gz = <SID>grey_number(a:b)

        " get the closest color
        let l:x = <SID>rgb_number(a:r)
        let l:y = <SID>rgb_number(a:g)
        let l:z = <SID>rgb_number(a:b)

        if l:gx == l:gy && l:gy == l:gz
            " there are two possibilities
            let l:dgr = <SID>grey_level(l:gx) - a:r
            let l:dgg = <SID>grey_level(l:gy) - a:g
            let l:dgb = <SID>grey_level(l:gz) - a:b
            let l:dgrey = (l:dgr * l:dgr) + (l:dgg * l:dgg) + (l:dgb * l:dgb)
            let l:dr = <SID>rgb_level(l:gx) - a:r
            let l:dg = <SID>rgb_level(l:gy) - a:g
            let l:db = <SID>rgb_level(l:gz) - a:b
            let l:drgb = (l:dr * l:dr) + (l:dg * l:dg) + (l:db * l:db)
            if l:dgrey < l:drgb
                " use the grey
                return <SID>grey_color(l:gx)
            else
                " use the color
                return <SID>rgb_color(l:x, l:y, l:z)
            endif
        else
            " only one possibility
            return <SID>rgb_color(l:x, l:y, l:z)
        endif
    endfun

    " returns the palette index to approximate the 'rrggbb' hex string
    fun <SID>rgb(rgb)
        let l:r = ("0x" . strpart(a:rgb, 0, 2)) + 0
        let l:g = ("0x" . strpart(a:rgb, 2, 2)) + 0
        let l:b = ("0x" . strpart(a:rgb, 4, 2)) + 0

        return <SID>color(l:r, l:g, l:b)
    endfun

    " sets the highlighting for the given group
    fun <SID>X(group, fg, bg, attr)
        if a:fg != ""
            exec "hi " . a:group . " guifg=#" . a:fg . " ctermfg=" . <SID>rgb(a:fg)
        endif
        if a:bg != ""
            exec "hi " . a:group . " guibg=#" . a:bg . " ctermbg=" . <SID>rgb(a:bg)
        endif
        if a:attr != ""
            exec "hi " . a:group . " gui=" . a:attr . " cterm=" . a:attr
        endif
    endfun

    call <SID>X("Normal", "000000", "ffdab9", "")

    call <SID>X("SpecialKey", "0000ff", "", "bold")
    call <SID>X("NonText", "0000ff", "", "bold")
    call <SID>X("Directory", "0000ff", "", "")
    call <SID>X("ErrorMsg", "ffffff", "ff0000", "bold")
    call <SID>X("IncSearch", "", "", "reverse")
    call <SID>X("Search", "000000", "eec900", "")
    call <SID>X("MoreMsg", "2e8b57", "", "bold")
    call <SID>X("ModeMsg", "", "", "bold")
    call <SID>X("LineNr", "cd0000", "", "")
    call <SID>X("Question", "2e8b57", "", "bold")
    call <SID>X("StatusLine", "ffffff", "000000", "bold")
    call <SID>X("StatusLineNC", "ffdab9", "737373", "bold")
    call <SID>X("VertSplit", "ffffff", "737373", "bold")
    call <SID>X("Title", "cd1076", "", "bold")
    call <SID>X("Visual", "cccccc", "000000", "reverse")
    call <SID>X("VisualNOS", "", "", "bold,underline")
    call <SID>X("WarningMsg", "ff0000", "", "bold")
    call <SID>X("WildMenu", "000000", "ffff00", "")
    call <SID>X("Folded", "000000", "e3c1a5", "")
    call <SID>X("FoldColumn", "00008b", "cccccc", "")
    call <SID>X("DiffAdd", "", "ffffff", "")
    call <SID>X("DiffChange", "", "edb5cd", "")
    call <SID>X("DiffDelete", "add8e6", "f6e8d0", "bold")
    call <SID>X("DiffText", "", "ff8060", "bold")
    call <SID>X("Cursor", "ffdab9", "000000", "")
    call <SID>X("lCursor", "ffdab9", "000000", "")
    call <SID>X("ColorColumn", "", "eeeeee", "NONE")
    call <SID>X("CursorLine", "", "eeeeee", "NONE")

    " Colors for syntax highlighting
    call <SID>X("Comment", "406090", "", "")
    call <SID>X("Constant", "c00058", "", "")
    call <SID>X("Special", "6a5acd", "", "")
    call <SID>X("Identifier", "008b8b", "", "")
    call <SID>X("Statement", "a52a2a", "", "bold")
    call <SID>X("PreProc", "cd00cd", "", "")
    call <SID>X("Type", "2e8b57", "", "bold")
    call <SID>X("Ignore", "ffdab9", "", "")
    call <SID>X("Error", "ffffff", "ff0000", "bold")
    call <SID>X("Todo", "0000ff", "ffff00", "")

    " delete functions {{{
    delf <SID>X
    delf <SID>rgb
    delf <SID>color
    delf <SID>rgb_color
    delf <SID>rgb_level
    delf <SID>rgb_number
    delf <SID>grey_color
    delf <SID>grey_level
    delf <SID>grey_number
    " }}}
endif
