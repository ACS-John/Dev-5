20000 ! Replace S:\acsTM\Support-CNV
20200   dim c$(4)*50
20400   open #1: "Name="&env$('Q')&"\TMmstr\Support.h50420,Version=1,KFName="&env$('Q')&"\TMmstr\Support-Idx.h50420,Use,RecL=246,KPs=11/17,KLn=6/2,Shr",internal,outIn,keyed 
20600   open #2: "Name="&env$('Q')&"\TMmstr\Support.h420,KFName="&env$('Q')&"\TMmstr\Support-Idx.h420,Replace,Version=2,RecL=246,KPs=1/7,KLn=6/2,Shr",internal,output,keyed 
20800   do 
21000     read #1,using FORM_1: id_a,id_b,client,id$,sd,tf$,_ed,c1,mat c$ eof END1
21200 FORM_1: form pos 1,n 5,n 5,n 6,c 2,n 8,c 2,n 8,n 10.2,4*c 50
21600     if client<>id_a then 
21800       id_a=val(str$(id_a)&str$(id_b)(1:1))
22000       id_b=val(str$(id_b)(2:999))
22200     end if  ! client<>id_a
22400 ! if client<>id_a then pr client,id_a : pause
22500 ! if id_b>20 then pr id_b;'>20' : pause
22520     id2=id_b
22600 ! id2=fp(id*.01)*100
22800     pr #0,using 'form pos 1,n 6,x 1,n 2,x 1,c 2,x 1,n 8,x 1,c 2,x 1,n 8,x 1,n 10.2': client,id2 ! ,id$,sd,tf$,_ed,c1
22820     if id2=0 then pr 'id2 is zero!' : pause 
23000     write #2,using FORM_OUT: client,id2,id$,sd,tf$,_ed,c1,mat c$
23200 FORM_OUT: form pos 1,n 6,n 2,c 2,n 8,c 2,n 8,n 10.2,4*c 50
23400     wctr=wctr+1
23600   loop 
23800 END1: stop 
