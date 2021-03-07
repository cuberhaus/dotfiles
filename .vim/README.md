## Clean Launch
```
vim --clean
```
## Veure els errors per pantalla
```
:messages
```
## Comment line
gc - toggles line comment. For example gcc to toggle line comment for current line and gc2j to toggle line comments for the current line and the next two lines.  
- VS CODE  
gC - toggles block comment. For example gCi) to comment out everything within parentheses.
## Change/Delete/Add surrounding like " " ' ' ( ) [ ] etc
| Surround Command	Description| |
|--|--|
| d s < existing char > |	Delete existing surround
| c s < existing char > < desired char >	| Change surround existing to desired
| y s < motion > < desired char >	|Surround something with something using motion (as in "you surround")
|S < desired char >	| Surround when in visual modes (surrounds full selection)
 
Open { or [ to add spaces in between and closing ] }  for no spaces: cs]{  
{ Hello } world!
## Jump around

Press Ctrl-O to jump back to the previous (older) location.
Press Ctrl-I (same as Tab) to jump forward to the next (newer) location.

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

## 🎩 VSCodeVim tricks!
VS Code has a lot of nifty tricks and we try to preserve some of them:

gd - jump to definition.  
gq - on a visual selection reflow and wordwrap blocks of text, preserving commenting style. Great for formatting documentation comments.  
gb - adds another cursor on the next word it finds which is the same as the word under the cursor.  
af - visual mode command which selects increasingly large blocks of text. For example, if you had "blah (foo [bar 'ba|z'])" then it would select 'baz' first. If you pressed af again, it'd then select [bar 'baz'], and if you did it a third time it would select "(foo [bar 'baz'])".  
gh - equivalent to hovering your mouse over wherever the cursor is. Handy for seeing types and error messages without reaching for the mouse!  


