00030   library 'S:\Core\Library': fnButton,fnTos,fnAcs,fnFra,fntop, fnCmdKey,fnbutton_or_disabled
00040   dim text$*80, tooltiptext$*200,cap$*128
00060   fntop(program$)
00080   fnTos(sn$="test-Button")
00090   fnFra(1,1,17,92,cap$) : curframe=1
00100   tooltiptext$ = "Hi, pleasure to meet you, I'm Mr. ToolTipText."
00110 ! h=12 : w=90 
00112   fnbutton_or_disabled(1,1,1,"Look, I'm a button",8,tooltiptext$,w,curframe)
00113   fnbutton_or_disabled(0,2,1,"Look, I'm a button",8,tooltiptext$,w,curframe)
00114   ! fnButton(1,1,"Look, I'm a button",8,tooltiptext$,h,w,curframe)
00120   fnCmdKey('every form needs one',1)
00130   fnAcs(sn$,0,mat response$,ck)
00140 ! 
00150   pr 'command key returned is '&str$(ck)
00160   stop 
