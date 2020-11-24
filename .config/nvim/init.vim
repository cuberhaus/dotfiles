set runtimepath^=~/.vim runtimepath+=~/.vim/after
let &packpath = &runtimepath
source ~/.vim/vimrc

function s:Hover()
    " get the doc string from YCM
    let response = youcompleteme#GetCommandResponse('GetDoc')
    if response == ''
        return
    endif
    " set the width
    let s:width = min([winwidth('%') * 9 / 10, 100])
    " calculate the height to show the whole doc with wrap enabled
    let lines = split(response, '\n')
    let s:height = len(lines) + 1
    for s:line in lines
        let s:height = s:height + len(s:line) / s:width
    endfor
    " nvim floating window interface
    let buf = nvim_create_buf(v:false, v:true)
    call nvim_buf_set_lines(buf, 0, -1, v:true, lines)
    let opts = {'relative': 'cursor', 'width': s:width, 'height': len(lines) + 1, 'col': 1,
                \ 'row': 0, 'anchor': 'SW', 'style': 'minimal'}
    let s:win = nvim_open_win(buf, 0, opts)
    " set the window option
    call nvim_win_set_option(s:win, 'winhl', 'Normal:NormalFloat')
    call nvim_win_set_option(s:win, 'wrap', v:true)
    call nvim_win_set_option(s:win, 'linebreak', v:true)
    " close the window once the cursor moved
    autocmd CursorMoved <buffer> ++once call nvim_win_close(s:win, v:false)
endfunction

command YcmGetDocFloatWin :call <SID>Hover()
autocmd FileType c,cpp,h,hpp nmap <leader>k :YcmGetDocFloatWin<cr>

