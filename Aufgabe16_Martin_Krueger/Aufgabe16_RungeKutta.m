(* ::Package:: *)

(* ::Chapter:: *)
(*Aufgabe 16 Runge-Kutta-Verfahren*)


(* ::Subsection:: *)
(*Aufgabenteil a)*)
(*Implementierung des Runge-Kutta-Verfahrens*)


(* ::Text:: *)
(*Diese Implementierung verwendet die selben Variablenbezeichnungen, wie in der Aufgabenstellung verwendet wurden.*)
(*Es werden auch vektorwertige Eingaben, also Differentialgleichungssysteme n-ter Ordnung unterst\[UDoubleDot]tzt. Letzteres wird insbesondere f\[UDoubleDot]r Aufgabenteil b) relevant.*)


(* ::Input:: *)
(*RungeKutta[phi_,t_,y_,t0_,y0_,intervall_,steps_]:= *)
(*(*Input vectors must be the same size*)*)
(*Module[{xList,yList,k1,k2,k3,k4,ti,yi,out},*)
(*yi=y0;*)
(**)
(*out={};*)
(*For[i=0,i<=(steps),i++,*)
(*ti=t0+intervall*i;*)
(*k1=phi/.Flatten[{t->ti,Thread[y->yi]}];*)
(*k2=phi/.Flatten[{t->ti+(intervall/2),Thread[y->yi+(intervall/2)*k1]}];*)
(*k3=phi/.Flatten[{t->ti+(intervall/2),Thread[y->yi+(intervall/2)*k2]}];*)
(*k4=phi/.Flatten[{t->ti+intervall,Thread[y->yi+intervall*k3]}];*)
(*yi=yi+(k1/6+k2/3+k3/3+k4/6)*intervall;*)
(*out=Append[out,yi];*)
(*];*)
(*out*)
(*]*)
(**)


(* ::Subsection:: *)
(*Aufgabenteil b)*)
(*Import und Setup*)


(* ::Text:: *)
(*F\[UDoubleDot]r diesen Aufgabenteil werden die in der Aufgabenstellung gegebenen Daten verwendet. Lediglich das Gewicht von Venus wurde korrigiert.*)
(*Folgende Funktionen sind so implementiert, dass das System beliebig erweitert werden kann, sofern die Eingabewerte die selbe Struktur besitzen.*)


(* ::Input:: *)
(*SetDirectory[NotebookDirectory[]];*)
(*DATA=Import["Sonnensystem.dat"];*)


(* ::Input:: *)
(*Grid[Prepend[DATA,{"Himmelsk\[ODoubleDot]rper","Masse in kg","x [km]","y [km]","z [km]", "Speed x", "Speed y", "Speed z"}],Frame->All,Background->{{LightGray,None},{Gray,None}}]*)


(* ::Input:: *)
(*NAMES=DATA[[All,1]];*)
(*MASS=DATA[[All,2]];*)
(*Pos0=DATA[[All,{3,4,5}]];*)
(*Speed0=DATA[[All,{6,7,8}]];*)


(* ::Subsection:: *)
(*Hilfsfunktionen*)


(* ::Text:: *)
(*Gravity() berechnet einen Beschleunigungsvektor f\[UDoubleDot]r jeden Planeten im System. *)


(* ::Input:: *)
(*Gravity[posVectors_,massVector_]:=*)
(*(*Input vectors must be the same size*)*)
(*Module[{length,i,j,pos=posVectors,mass=massVector,gravityCONST,out},*)
(*out={};*)
(*gravityCONST=6.674*10^(-20);*)
(*length=Length[pos];*)
(*For[i=1,i<=length,i++,*)
(*out=Append[out,Evaluate[Sum[gravityCONST*mass[[i]]*mass[[j]]*(pos[[j]]-pos[[i]])/(Piecewise[{{1,Norm[(pos[[j]]-pos[[i]])]^3==0}},Norm[(pos[[j]]-pos[[i]])]^3]),{j,1,length}]]]*)
(**)
(*];*)
(**)
(*out*)
(*]*)


(* ::Input:: *)
(**)
(**)


(* ::Text:: *)
(*Sonnensystem f\[UDoubleDot]hrt f\[UDoubleDot]r das Sonnensystem das Runge-Kutta-Vefahren durch. Anschlie\[SZ]end werden die Positionsvektoren aus der Ergebnismenge extrahiert und die Impulsvektoren verworfen.*)


(* ::Input:: *)
(*SonnenSystem[pos0Vectors_,Impulse0_,mass_,precision_,steps_]:=*)
(*(*Input vectors must be the same size*)*)
(*Module[{pp,rr,p,r,phi,px,py,pz,rx,ry,rz,data,t,length,,i,k,tmp,out},*)
(*length=Length[pos0Vectors];*)
(*pp[i_]={Subscript[px, i][t],Subscript[py, i][t],Subscript[pz, i][t]};*)
(*rr[i_]={Subscript[rx, i][t],Subscript[ry, i][t],Subscript[rz, i][t]};*)
(*p[t_]=Array[pp,length];*)
(*r[t_]=Array[rr,length];*)
(*phi[t_]:=Flatten[{(1/MASS)*p[t],Flatten[Gravity[r[t],MASS]]}];*)
(*data:=RungeKutta[phi[t],t,Flatten[{r[t],p[t]}],0,Flatten[{Pos0,Impulse0}],precision,steps];*)
(*data=data[[All,1;;(length*3)]];(*removing impulse vectors from the set*)*)
(*out={};*)
(*For[i=1,i<=steps+1,i++,(*(steps+1) because RungeKutta includes set of y0"*)*)
(*tmp={};*)
(*For[k=0,k<length,k++,*)
(*tmp =Append[tmp,data[[i,(3*k+1);;(3*k+3)]]];*)
(**)
(*];*)
(*out=Append[out,tmp];*)
(*];*)
(*out*)
(*];*)


(* ::Input:: *)
(**)


(* ::Text:: *)
(*DynamicSonnensystemAnimation() erstellt eine Animation. F\[UDoubleDot]r die Berechnung der Daten wird die Hilfsfunktion Sonnensystem() verwendet.*)


(* ::Input:: *)
(*(*km/s,km,years*)*)


(* ::Input:: *)
(*DynamicSonnensystemAnimation[planetNames_,pos0Vectors_,speed0Vectors_,mass_,years_,evaluationsteps_,plotRange_]:=*)
(*(*Input vectors must be the same size*)*)
(*Module[{r,impulse0Vectors,intervall},*)
(**)
(*impulse0Vectors=mass*speed0Vectors;*)
(*intervall=(years*365*24*60*60)/evaluationsteps;*)
(*Clear[dataset,INTERVALL,colors];*)
(*dataset={};*)
(*INTERVALL=intervall;*)
(**)
(*dataset=SonnenSystem[pos0Vectors,impulse0Vectors,mass,intervall,evaluationsteps];*)
(**)
(*plotrange=plotRange;*)
(*Manipulate[*)
(**)
(**)
(*Graphics3D[Dynamic@*)
(*(*produces errors as long as calculation of dataset is running. Does not affect end result*)*)
(**)
(*Table[{ColorData["DarkBands"][c/Length[planetNames]],PointSize[.005],Point[dataset[[t,c]]]},{c,1,Length[planetNames]}],*)
(**)
(*Axes->True,*)
(*AxesStyle->Directive[FontSize->17,FontFamily->"Helvetica"],*)
(*AxesLabel->{"x","y","z"},BoxRatios->{1,1,1},*)
(*ImageSize->1000,PlotRange->plotrange,SphericalRegion->True],*)
(*Row[{Control[{t,1,evaluationsteps,1,Animator,ImageSize->Small,*)
(*AnimationRunning->False,AnimationRate->10,AnimationRepetitions->1}],Spacer[10],*)
(*Dynamic["Day no. "<> ToString[ Round[(((t-1)*INTERVALL)/(60*60*24))]]],"            "Grid[{Flatten[Table[{ColorData["DarkBands"][c/Length[planetNames]]},{c,1,Length[planetNames]}]],NAMES}]}],*)
(*FrameMargins->0*)
(**)
(*]*)
(*]*)


(* ::Text:: *)
(**)
(*Demo mit allen Anfangswerten:*)


(* ::Input:: *)
(*(*IMPORTANT: Enable Dynamic Updating*)*)


(* ::Input:: *)
(*DynamicSonnensystemAnimation[NAMES,Pos0,Speed0,MASS,2,500,4.5*10^9]*)


(* ::Text:: *)
(*Export der Animation als Video in das Verzeichnis dieses Notebooks*)


(* ::Input:: *)
(*Export["Sonnensystem.avi",DynamicSonnensystemAnimation[NAMES,Pos0,Speed0,MASS,1,100,3*10^9]]*)


(* ::Input:: *)
(*dataset[[400,1;;10]]*)
