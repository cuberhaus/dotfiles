## Clean Launch
```
vim --clean
```
## Veure els errors per pantalla
```
:messages
```
## Comment line
gcc per comentar gc per fer moviments
## Change/Delete/Add surrounding like " " ' ' ( ) [ ] etc
vim repeat cs'" to change surround from ' to "  
" ysiw" to add " to a word  
" ds" to remove surround  
## Jump around
Jump to category under the cursor<C-]>  
Jump back <C-T>
  
Press Enter to jump to the subject (topic) under the cursor.  
Press Backspace to return from the last jump.  
```
map <buffer> <CR> <C-]>  
map <buffer> <BS> <C-T>  
```
## Re-select last visual select
```
gv
```

## Increase numbers increasingly by 1
```
g ctrl A 
```
(increase i+1)

## Surround Visually:
Visual select then press S (and whatever you want to surround with)

## Save session
to start recording vim session
```
:Obsess
```
to source the session
```
vim -S
```


