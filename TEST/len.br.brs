00010 ! test\len
00020   dim x$*80
00030   let x$="asdf               "&chr$(0)&"                "
00040   pr len(x$)
00050   let x$=trim$(x$)
00060   pr len(x$)
00070   pause 
