10000   print newpage
10002   dim name$(1)*80
10100   open #1: 'Name=z:\acs\sg_name.txt',display,input 
10200   do 
10300     let name_item+=1
10400     mat name$(name_item)
10500     linput #1: name$(name_item) eof EO_NAME
10600   loop 
10700 EO_NAME: ! 
10800   for xtime=1 to 40
10900     let name1=int(rnd*udim(mat name$))+1
11000     let name2=int(rnd*udim(mat name$))+1
11200     print name$(name1)&' '&name$(name2)
11300   next xtime
