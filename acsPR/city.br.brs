05590 CSZ: ! EXTRACT  CITY$,STATE$,ZIP$ FORM CSZ$
05600 L5600: let p1=pos(csz$,".",1)
05610   if p1>0 then let csz$(p1:p1)=" ": let p2=p1: goto L5600
05620 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN LET CSZ$(P2+1:P2+1)="" ! dump any extra spaces
05630   let p2=0
05640 L5640: let p1=pos(csz$,",",1)
05650   if p1>0 then let csz$(p1:p1)=" ": let p2=p1: goto L5640
05660 ! IF P2>0 AND CSZ$(P2+1:P2+1)=" " THEN LET CSZ$(P2+1:P2+1)="" ! dump any extra spaces
05670 L5670: let p1=pos(rtrm$(csz$),"  ",1)
05680   if p1>0 then let csz$(p1+1:p1+1)="" : goto L5670
05690   let csz$=ltrm$(rtrm$(csz$)): let p1=pos(csz$," ",-1)
05700   let zip$=csz$(p1+1:len(csz$)): let zip$=ltrm$(rtrm$(zip$))
05710   let p2=pos(csz$(1:p1-1)," ",-1) : let state$=csz$(p2+1:p1-1)(1:2) : let state$=ltrm$(rtrm$(state$))
05720   let city$=csz$(1:p2-1)(1:15): let city$=ltrm$(rtrm$(city$))
05730   return 
