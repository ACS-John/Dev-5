00030   library 'S:\Core\Library': fncno
00040   let fncno(cno,cnam$)
00120   dim ar(5),ph2$*12,ss2$*11,arta(2),des$*30,cm$*70,cnam$*40
00130   dim z$*5,a$(5)*30,ph$*12,ss$*11,dd(10),sc(10),ca(10),ta(25,2),fb(25)
00140   dim z$(3)*5,ph$(6)*12,ss$(6)*11,h$(3,6)*30,l(3,10),m(3,10),pno(3),mye(3),har(3,5)
00150   dim fl1$(33),oi1$(38),scr1$(31)*20,scrid$(2)*40
00160   dim k$*5,d$*9,b(8),sc$*4,iv$*12,fl2$(15),scr2$(14)*25,ot2$(14),in2$(14)
00170   dim flit$(6)*18,scrt$(6)*20,scrz$(2)*79,desc$(8)*18
00180   dim hlp$(20)*78,flh$(22)*18,hhd$*60
00190   dim p$*5,iv$*12,tr(6),id$*20 ,cat$(10)*30
00195   dim st$(20)*24,scot$(21),sct$(40),app(20),ma(20)
00196   dim st2$(20)*24,ap2(20),ma2(20)
00260   open #1: "Name="&env$('Q')&"\TMmstr\CLmstr.H"&str$(cno)&",KFName="&env$('Q')&"\TMmstr\CLIndex.H"&str$(cno)&",Shr",internal,outin,keyed 
00730 L730: read #1,using L740: z$,mat a$,ph$,ss$,pno,mye,mat dd,mat sc,mat ca,ph2$,ss2$,mat ar,mat arta,cm$,mat app,mat ma,mat ap2,mat ma2 eof END1
00740 L740: form pos 1,c 5,5*c 30,c 12,c 11,n 9,n 2,10*pd 3,10*n 1,10*pd 3,c 12,c 11,2*pd 5.2,pd 4.3,2*n 1,2*pd 3,c 70,20*n 1,20*pd 3.2,20*n 1,20*pd 3.2
00750   if app(4)=0 then goto L730
00760   pr #255: z$;" ";a$(1),ph$
00770   goto L730
00780 END1: stop 
