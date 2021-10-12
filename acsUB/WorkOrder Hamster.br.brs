autoLibrary
on error goto Ertn
gosub Enum
fnTop(program$)
fnH2Init
fnH2AddText("Account  "       ,10              ,'C'                                    )
! fnH2AddText("Date"            ,8,'N',8,mask_number )
fnH2AddText("Date"            ,textlen_ccyymmdd,'N',storage_len_ccyymmdd,mask_ccyymmdd )
fnH2AddText("e$(2)(1:30)"     ,30              ,'C'                                    )
fnH2AddText("line$(1)"        ,100             ,'C'                                    )
fnH2AddText("line$(2)"        ,100             ,'C'                                    )
fnH2AddText("line$(3)"        ,100             ,'C'                                    )
fnH2AddText("line$(4)"        ,100             ,'C'                                    )
! fnH2AddText("line$(5)"        ,100             ,'C'                                    ) ! adding this line causes err 58 - at least with Pennington it does
fnH2AddComboF(1,'[Q]\UBmstr\Customer.h[cno]',1,10,41,30,'[Q]\UBmstr\ubIndex.h[cno]',0)

open #1: "Name=[Q]\UBmstr\workorder.h[cno],Use,RecL=600,Shr",i,outi,r
fnHamster2("workorder")
close #1:
Xit: fnXit
include: ertn
include: Enum
