00010 ! Replace test\Label3.br
00015 ! -------------------------------------------------------------------
00020   library 'S:\Core\Library': fnprg,fnLbl,fnTos,fnAcs
00030 ! -------------------------------------------------------------------
00040   dim mytext$*50,filename$*50
00050   prg$="test\Label3.br" !:
        fnprg(prg$,2)
00055 ! -------------------------------------------------------------------
00060   fnTos(sn$='label3')
00070   myline = 1 : mypos = 5 : mylen = 10 : myalign = 2 !:
        font_mod=0 !:
        mytext$="This is a two Line Label, Woo Hoo."
00075   fnLbl(myline,mypos,mytext$,mylen,myalign,font_mod)
00080   fnAcs(sn$,0,2,mat response$,ckey)
