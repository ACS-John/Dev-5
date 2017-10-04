10000   library "S:\Core\Library.br": fncno,fnxit
10010   dim z$*10,a$*10,srch$*10,ru(12),charge(10),bd(10),tg(10)
10020   fncno(cno)
10030   open #(h_mstr:=1): "Name="&env$('Q')&"\UBmstr\customer.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubindex.h"&str$(cno)&",SHR",internal,outin,keyed 
10040   open #(h_trans:=11): "Name="&env$('Q')&"\UBmstr\ubtransvb.h"&str$(cno)&",KFName="&env$('Q')&"\UBmstr\ubtrindx.h"&str$(cno)&",SHR",internal,outin,keyed 
10050 MSTRFORM: form c 10,pos 217,12*pd 5,pos 292,pd 4.2,pd 4,12*pd 4.2,pos 388,10*pd 5.2,pos 1741,n 2
10060 TRANSFORM: form c 10,n 8,n 1,12*pd 4.2,2*pd 5,pos 98,pd 4.2,n 1
10070   do 
10080 NEXTCUST: read #h_mstr,using MSTRFORM: z$,mat ru,bal,cdate,mat charge,net,gross,mat bd,rt eof XIT
10090     if rt=3 then 
10100       let got_bal=0 : let got_current=0 : let got_prior=0 : let has_bad=0 : collections=0
10110       let srch$=lpad$(cnvrt$("PIC(######.##)",val(z$)+.01),10," ")
10120       read #h_trans,using TRANSFORM,search>=srch$: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode eof PROCESSCUST
10130 PROCESSCUST: do 
10140         read #h_trans,using TRANSFORM,prior: a$,tdate,tcode,tamt,mat tg,tnet,wread,wused,tbal,pcode eof NEXTCUST
10150         if a$<>z$ then goto NEXTCUST
10160         if tdate=20130405 then ! this is one of the bad transactions
10170           let has_bad=1
10180 ! edit breakdowns
10190           if tcode=3 or tcode=4 then ! C/M or collection
10200             for j=1 to 10 : bd(j)+=tg(j) : next j
10210           else if tcode=1 and pcode<>4 then ! Charge
10220             for j=1 to 10 : bd(j)-=tg(j) : next j
10230           end if 
10240           delete #h_trans: ! delete the bad transaction
10250         else if tdate<20130405 and has_bad=1 then 
10260           if got_bal=0 then 
10270             bal=tbal : let got_bal=1 ! get the balance from the most recent transaction before the bad ones
10280           end if 
10290           if tcode=3 then ! this is a collection, so we'll need to make sure this is a correct balance
10300             collections+=tamt
10310           end if 
10320           if tcode=1 and pcode<>4 then ! this is a charge
10330             if got_current=0 then ! this is the current charge
10340               cdate=fn_date6(tdate)
10350               let ru(4)=ru(4)-wused : let ru(1)=wread : let ru(3)=wused
10360               let net=tnet : let gross=tnet+tg(10)
10370               mat charge=tg
10380               if tbal-collections<>bal then bal=tbal-collections
10390               let got_current=1
10400             else ! this is the prior charge
10410               let ru(2)=wread
10420               let got_prior=1
10430             end if 
10440           end if 
10450           if got_bal and got_current and got_prior then 
10460 ! adjust breakdowns if necessary
10470             for j=1 to 10
10480               if bd(j)<0 then bd(j)=0
10490             next j
10500             for j=1 to 10
10510               if sum(bd)>bal then bd(j)-=min(bd(j),sum(bd)-bal)
10520             next j
10530             if sum(bd)<>bal then bd(8)+=bal-sum(bd)
10540 ! write the data back to the customer record
10550             rewrite #h_mstr,using MSTRFORM: z$,mat ru,bal,cdate,mat charge,net,gross,mat bd,rt
10560             goto NEXTCUST
10570           end if 
10580         end if 
10590       loop 
10600     end if 
10610   loop 
10620   def fn_date6(date8)
10630     dim date8$*8
10640     let date8$=str$(date8)
10650     let date8$=date8$(5:6)&date8$(7:8)&date8$(3:4)
10660     fn_date6=val(date8$)
10670   fnend 
10680 XIT: let fnxit
