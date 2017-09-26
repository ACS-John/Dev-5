00010 ! Replace test\Label3.br
00015 ! -------------------------------------------------------------------
00020   library 'S:\Core\Library': fnprg,fnlbl,fntos,fnacs
00030 ! -------------------------------------------------------------------
00040   dim mytext$*50,filename$*50
00050   let prg$="test\Label3.br" !:
        let fnprg(prg$,2)
00055 ! -------------------------------------------------------------------
00060   let fntos(sn$='label3')
00070   let myline = 1 : let mypos = 5 : let mylen = 10 : let myalign = 2 !:
        let font_mod=0 !:
        let mytext$="This is a two Line Label, Woo Hoo."
00075   let fnlbl(myline,mypos,mytext$,mylen,myalign,font_mod)
00080   let fnacs(sn$,0,2,mat response$,ckey)
