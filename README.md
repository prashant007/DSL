# A DSL for explainable AHP
A DSL for encoding AHP problems as well as generating explanations for the output that is generated.

This package is an executable, i.e it can be used when installed. The installation step of the package are as follows:

1) Download the package and navigate to the folder of the package containing the cabal file.
2) Execute "cabal configure"
3) Execute "cabal install" 
4) The package is now installed and can be used in a Haskell file like a normal import. For example - if we wanted to use the functions for MDS explanations (Explanation.MDS) then we can import it as shown below:

    import Explanation.MDS

===================================================

The file Car.hs shows an encoding of an AHP problem pertaining to the decision of buying of a car. It also shows how to generate an explanation for the decision output by the AHP. The file SensitivityExampleCar.hs illustrates the functions for sensitivity analysis of the same problem. A description of the functionalities of the DSL using the car example is provided in the wiki page of the DSL. 

