autoLibrary

open #20: "Name=[Q]\UBmstr\label"&wsid$&".txt,Replace,RecL=5000",d,o 
pr #20: 'Call Print.MyOrientation("Portrait")'
lyne=4

bc$="123456789012"
label_pos=1 : lyne=16
if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(label_pos)&','&str$(lyne+ymargin)&',"'&bc$&'")'

close #1: ioerr ignore
close #3: ioerr ignore
fnpa_finis

stop 
