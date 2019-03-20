 ! function\enterselectionlistview.brs
 ! Created on 07/13/2009
 !
 ! fnEnterSelectionListview - This Function Primes the Selection Listview
 !
 !
 dim MarkedRecords$(1)

 def fnEnterSelectionListview(ParentKey$,DataFile;___,Index,KeyLen)

    keyLen=kln(DataFile)
    mat MarkedRecords$(int(len(ParentKey$)/KeyLen))
    
    for Index=1 to udim(mat MarkedRecords$)
       markedRecords$(Index)=ParentKey$(((Index-1)*KeyLen)+1:(Index*KeyLen))
    next Index
 fnend
