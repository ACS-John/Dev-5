00010 ! Replace test\CmbCNo
00020 ! -----------------------------------------------------------------------
00030   library 'S:\Core\Library': fncmbcno,fnAcs,fnTos,fntop,fnCmdKey,fncursys$
00040   dim resp$(1)*80
00050 ! -----------------------------------------------------------------------
00060   fncursys$(cursys$='UB') !:
        ! sets the current system to Utility Billing
00070   fntop("test\CmbCNo","Test Combo Company Number")
00080 ! -----------------------------------------------------------------------
00090   fnTos(sn$="CmbCNo")
00100   fncmbcno(1,1) !:
        resp$(1)="1"
00110   fnCmdKey("Ok",1,1,1)
00120   fnAcs(sn$,0,mat resp$,ck)
00130 ! -----------------------------------------------------------------------
00140   pr resp$(1)(43:47)
