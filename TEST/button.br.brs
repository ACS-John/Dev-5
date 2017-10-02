00030   library 'S:\Core\Library': fnbutton,fntos,fnacs,fnfra,fntop, fncmdkey,fnbutton_or_disabled
00040   dim text$*80, tooltiptext$*200,cap$*128
00060   let fntop(program$)
00080   let fntos(sn$="test-Button")
00090   let fnfra(1,1,17,92,cap$) : curframe=1
00100   let tooltiptext$ = "Hi, pleasure to meet you, I'm Mr. ToolTipText."
00110 ! Let H=12 : Let W=90 
00112   let fnbutton_or_disabled(1,1,1,"Look, I'm a button",8,tooltiptext$,w,curframe)
00113   let fnbutton_or_disabled(0,2,1,"Look, I'm a button",8,tooltiptext$,w,curframe)
00114   ! fnbutton(1,1,"Look, I'm a button",8,tooltiptext$,h,w,curframe)
00120   let fncmdkey('every form needs one',1)
00130   let fnacs(sn$,0,mat response$,ck)
00140 ! 
00150   pr 'command key returned is '&str$(ck)
00160   stop 
