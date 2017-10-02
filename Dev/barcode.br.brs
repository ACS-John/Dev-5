00010   gosub VBOPENPRINT
00020   bc$="123456789012"
00025   let label_pos=1 : let lyne=16
00030   if trim$(bc$)<>"" then pr #20: 'Call Print.DisplayBarCode('&str$(label_pos)&','&str$(lyne+ymargin)&',"'&bc$&'")'
00040   gosub RELEASE_PRINT
00050   stop 
00060 VBOPENPRINT: ! 
00070   if file(20)=-1 then 
00080     open #20: "Name="&env$('Q')&"\UBmstr\label"&wsid$&".txt,Replace,RecL=5000",display,output 
00090     pr #20: 'Call Print.MyOrientation("Portrait")'
00100     let lyne=4
00110   end if 
00120   return 
00130 RELEASE_PRINT: library 'S:\Core\Library': fnpa_finis
00140   close #1: ioerr L150
00150 L150: close #3: ioerr L160
00160 L160: let fnpa_finis
00180   return 
