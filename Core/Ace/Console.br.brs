00010 ! Replace S:\Core\Ace\Console.br
00020 ! function to hide/show the BR Console
00030 ! ______________________________________________________________________
00040   def library fnconsole(;on_off)
00050     option retain 
00070     dim cap$*128
00080     on=1 : off=0
00090     if on_off=windowstate then goto XIT !   : pr "WindowState remains the same.
00110     if on_off=off then 
00120       execute 'Config GUI On'
00130     else if on_off=on then 
00140       execute 'Config GUI off'
00150     end if 
00320     goto XIT
00330 ! ______________________________________________________________________
00340 XIT: let windowstate=on_off
00350   fnend 
