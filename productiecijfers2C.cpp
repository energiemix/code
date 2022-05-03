#include <Rcpp.h>
using namespace Rcpp;

// [[Rcpp::export]]
DataFrame prodcijfersC(DataFrame mix, double startwaarde, double efficientie_H2_naar_El, double efficientie_El_naar_H2, double opslag_pct_jr){
	int nrow = mix.nrow();
	double overloop = 0;
    
	//maak nieuwe vectoren aan voor delta data
	NumericVector hulpbron(nrow);
	NumericVector opslag(nrow);
	NumericVector onbenut(nrow);
	NumericVector opgeslagen(nrow);
	NumericVector laadtoestand(nrow);
	NumericVector verlies_elproductie(nrow);
	
	//zorg dat de juiste delen van de Dataframe aanspreekbaar zijn
    NumericVector tekort_el       = mix["tekort.el"];
	NumericVector tekort_h2       = mix["tekort.h2"];
	NumericVector surplus         = mix["surplus"];
	NumericVector verlies_h2productie = mix["verlies.h2productie"];
	
	laadtoestand(0) = startwaarde; // beginwaarde laadtoestand accu
	for (int i = 0; i < nrow; i++) {
		//Rcout << "The value of v : " << laadtoestand << "\n";
		
		// Bediening directe vraag is uitgerekend in R. Code hier gaat over aanvullen van tekorten.
		
		// I.
		//Bedien de stroomvraag als eerste, want omzetting H2 naar stroom heeft 50% verlies
		//Het tekort aan stroom wordt als eerste zoveel mogelijk uit de opslag gehaald
		//Is er geen opslag, gebruik dan de hulpbron
		if (laadtoestand(i) > tekort_el(i)/efficientie_H2_naar_El) {
		  // Er is voldoende opslag om het tekort aan te vullen.
		  opslag(i)              = tekort_el(i);
		  verlies_elproductie(i) = tekort_el(i)/efficientie_H2_naar_El - tekort_el(i);
		  laadtoestand(i)        = laadtoestand(i) - tekort_el(i)/efficientie_H2_naar_El;
		} else {
		  // Er is onvoldoende opslag om het tekort aan te vullen.
		  // gebruik wat er resteert in de opslag...
		  opslag(i)              = laadtoestand(i) * efficientie_H2_naar_El;
		  verlies_elproductie(i) = laadtoestand(i) * (1-efficientie_H2_naar_El);
		  // ... en zet de hulpbron in voor het overige
		  hulpbron(i)            = tekort_el(i) - opslag(i);
		  // geen verlies hier!
		  laadtoestand(i)        = 0;
		}
		
		// II.
		//van wat er over is in de opslag, wordt gepoogd het tekort aan H2-vraag te voldoen
		// DIT GELDT ALLEEN IN HET GEVAL VAN EEN H2-OPSLAG!!!!!! 
		// Li-OPSLAG HEEFT ANDERE EFFCIENTIES!!!! TO DO!
		if (laadtoestand(i) >= tekort_h2(i)) {
		  // Er is voldoende 
		  opslag(i)              = opslag(i) + tekort_h2(i);
		  // geen verlies!
		  laadtoestand(i)        = laadtoestand(i) - tekort_h2(i);
		} else {
		  opslag(i)              = opslag(i) + laadtoestand(i);
		  // geen verlies!
		  
		  // en de hulpbron moet worden ingezet
		  hulpbron(i)            = hulpbron(i) + tekort_h2(i) - laadtoestand(i);
		  verlies_h2productie(i) = verlies_h2productie(i) + (hulpbron(i)/efficientie_El_naar_H2 - hulpbron(i));
		  laadtoestand(i)        = 0;
		}
		
		// het surplus vult de opslag - voor de volgende ronde
		if (i < (nrow-1)) { // Zero-based arrays!
		  overloop = (laadtoestand(i) + surplus(i) * efficientie_El_naar_H2) - opslag_pct_jr;
		  if (overloop > 0) {
			//oeps, opslag loopt over...			  
			laadtoestand(i+1)      = opslag_pct_jr;  // voller kan het niet
			verlies_h2productie(i) = verlies_h2productie(i) + 
									 (opslag_pct_jr-laadtoestand(i))/efficientie_El_naar_H2 * (1-efficientie_El_naar_H2);
			onbenut(i)             = surplus(i) - ((opslag_pct_jr-laadtoestand(i))/efficientie_El_naar_H2);
			opgeslagen(i+1)        = opslag_pct_jr-laadtoestand(i);
		  } else {
			// niets aan de hand, geen overloop
			laadtoestand(i+1)      = laadtoestand(i) + surplus(i) * efficientie_El_naar_H2;
			verlies_h2productie(i) = verlies_h2productie(i) + (surplus(i) * (1-efficientie_El_naar_H2));
			onbenut(i)             = 0;
			opgeslagen(i+1)        = surplus(i) * efficientie_El_naar_H2;
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
		Rcpp::Named("verlies.elproductie") = verlies_elproductie,
		Rcpp::Named("verlies.h2productie")   = verlies_h2productie);

	return(NDF);
 }