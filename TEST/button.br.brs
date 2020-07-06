autoLibrary
dim text$*80, tooltiptext$*200,cap$*128
fnTop(program$)
fnTos
fnFra(1,1,17,92,cap$) : curframe=1
tooltiptext$ = "Hi, pleasure to meet you, I'm Mr. ToolTipText."
! h=12 : w=90 
fnbutton_or_disabled(1,1,1,"Look, I'm a button",8,tooltiptext$,w,curframe)
fnbutton_or_disabled(0,2,1,"Look, I'm a button",8,tooltiptext$,w,curframe)
! fnButton(1,1,"Look, I'm a button",8,tooltiptext$,h,w,curframe)
fnCmdKey('every form needs one',1)
fnAcs(mat response$,ckey)

pr 'command key returned is '&str$(ckey)
stop 
