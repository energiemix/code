#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame prodcijfersC(DataFrame mix, double startwaarde, double eff_R1_W, double eff_R2_O, double eff_H_W, double eff_O_W, double eff_O_E, double opslag_pct_jr){
	// O = opslag, E = vraag naar elektriciteit, W = vraag naar waterstof, R1 = restant energie na aftrek elektriciteitsvraag, R2 = restant energie na aftrek elek en waterstofbehoefte, H = hulpbron
	int nrow = mix.nrow();
	double overloop = 0;
    
	//maak nieuwe vectoren aan voor delta data
	NumericVector hulpbron(nrow);
	NumericVector opslag(nrow);
	NumericVector onbenut(nrow);
	NumericVector opgeslagen(nrow);
	NumericVector laadtoestand(nrow);
	
	//zorg dat de juiste delen van de Dataframe aanspreekbaar zijn
    NumericVector tekort_el       = mix["tekort.el"];
	NumericVector tekort_h2       = mix["tekort.h2"];
	NumericVector surplus         = mix["surplus"];
	NumericVector eff_verlies     = mix["eff.verlies"];
	
	laadtoestand(0) = startwaarde; // beginwaarde laadtoestand accu
	for (int i = 0; i < nrow; i++) {
		//Rcout << "The value of v : " << laadtoestand << "\n";
		
		// Bediening directe vraag is uitgerekend in R. Code hier gaat over aanvullen van tekorten.
		
		// I.
		//Bedien de stroomvraag als eerste, want omzetting H2 naar stroom heeft 50% verlies
		//Het tekort aan stroom wordt als eerste zoveel mogelijk uit de opslag gehaald
		//Is er geen opslag, gebruik dan de hulpbron
		if (laadtoestand(i) > tekort_el(i)/eff_O_E) {
		  // Er is voldoende opslag om het tekort aan te vullen.
		  opslag(i)              = tekort_el(i);  // opslag(i) betekent netto uit opslag geleverd.  
		  eff_verlies(i)         = eff_verlies(i) + tekort_el(i)/eff_O_E - tekort_el(i);
		  laadtoestand(i)        = laadtoestand(i) - tekort_el(i)/eff_O_E;
		} else {
		  // Er is onvoldoende opslag om het tekort aan te vullen.
		  // gebruik wat er resteert in de opslag...
		  opslag(i)              = laadtoestand(i) * eff_O_E;
		  eff_verlies(i)         = eff_verlies(i) + laadtoestand(i) * (1-eff_O_E);
		  // ... en zet de hulpbron in voor het overige
		  hulpbron(i)            = tekort_el(i) - opslag(i);
		  // geen verlies hier!
		  laadtoestand(i)        = 0;
		}
		
		// II.
		//van wat er over is in de opslag, wordt gepoogd het tekort aan H2-vraag te voldoen
		if (laadtoestand(i) >= tekort_h2(i)/eff_O_W) {
		  // Er is voldoende opslag om het tekort aan te vullen.
		  opslag(i)              = opslag(i) + tekort_h2(i);
		  eff_verlies(i)         = eff_verlies(i) + tekort_h2(i)/eff_O_W - tekort_h2(i);
		  laadtoestand(i)        = laadtoestand(i) - tekort_h2(i)/eff_O_W;
		} else {
		  // Er is onvoldoende opslag om het tekort aan te vullen.
		  // gebruik wat er resteert in de opslag...
		  opslag(i)              = opslag(i) + laadtoestand(i) * eff_O_W;
		  eff_verlies(i)         = eff_verlies(i) + laadtoestand(i) * (1-eff_O_W); 
		  
		  // ... en zet de hulpbron in voor het overige
		  hulpbron(i)            = hulpbron(i) + tekort_h2(i) - laadtoestand(i) * eff_H_W;
		  eff_verlies(i)         = eff_verlies(i) + (hulpbron(i)/eff_H_W - hulpbron(i));
		  laadtoestand(i)        = 0;
		}
		
		// het surplus vult de opslag - voor de volgende ronde
		if (i < (nrow-1)) { // Zero-based arrays!
		  overloop = (laadtoestand(i) + surplus(i) * eff_R2_O) - opslag_pct_jr;
		  if (overloop > 0) {
			//oeps, de opslag loopt over...			  
			laadtoestand(i+1)      = opslag_pct_jr;  // voller kan het niet
			eff_verlies(i)         = eff_verlies(i) +
									 (opslag_pct_jr-laadtoestand(i))/eff_R2_O * (1-eff_R2_O);
			onbenut(i)             = surplus(i) - ((opslag_pct_jr-laadtoestand(i))/eff_R2_O);
			opgeslagen(i+1)        = opslag_pct_jr-laadtoestand(i);
		  } else {
			// niets aan de hand, geen overloop
			laadtoestand(i+1)      = laadtoestand(i) + surplus(i) * eff_R2_O;
			eff_verlies(i)         = eff_verlies(i) + (surplus(i) * (1-eff_R2_O));
			onbenut(i)             = 0;
			opgeslagen(i+1)        = surplus(i) * eff_R2_O;
	      }		
		}
    }
  
	// create returning data frame with the delta data
	Rcpp::DataFrame NDF = Rcpp::DataFrame::create(
		Rcpp::Named("hulpbron")         = hulpbron,
		Rcpp::Named("opslag")           = opslag,
		Rcpp::Named("onbenut")          = onbenut,
		Rcpp::Named("opgeslagen")       = opgeslagen,
		Rcpp::Named("laadtoestand")     = laadtoestand,
		Rcpp::Named("eff.verlies")      = eff_verlies);

	return(NDF);
 }
 
 

  
  
  
  