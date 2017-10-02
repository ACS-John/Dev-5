30000   pr newpage
30020   dim car$(3)*20,selectedcar$*20
30040 ! 
30060   car$(1)="BMW i8"
30080   car$(2)="Chrysler"
30100   car$(3)="BMW"
30120 ! 
30140   pr fields "10,10,20/combo 20,=,Select": mat car$
30160   rinput fields "10,10,20/combo 20,T[textboxes],Select": selectedcar$
40000   pr selectedcar$
