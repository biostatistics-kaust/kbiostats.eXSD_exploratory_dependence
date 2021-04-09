get_spectrum_params <- function(input){
	list(
		estimation_method=switch(input$lstCategorySpecEstimation,
			"param"=input$lstParamEstimation,
			"nonparam"=input$lstNonparamEstimation,
			"semiparam"="GSE"
		),
		VAR_order=switch(input$lstCategorySpecEstimation,
			"param"=input$txtParamVarOrder,
			"nonparam"=1,
			"semiparam"=input$txtSemiVarOrder
		),
		win=switch(input$lstCategorySpecEstimation,
			"param"="",
			"nonparam"=input$lstNonparamWindowType,
			"semiparam"=input$lstSemiWindowType
		),
		n_trials=switch(input$lstCategorySpecEstimation,
			"param"=input$txtParamVarOrder,
			"nonparam"=switch(input$lstNonparamEstimation,
				"PURE"=input$numPureTrials,
				"FFT"=1
			),
			"semiparam"=input$numSemiTrials
		),
		spans=as.numeric(switch(input$lstCategorySpecEstimation,
			"param"=1,
			"nonparam"=switch(input$lstNonparamEstimation,
				"PURE"=input$lstPureSpans,
				"FFT"=c(input$numFFTSpans)
			),
			"semiparam"=input$lstSemiSpans
		)),
		variance_window=switch(input$lstCategorySpecEstimation,
			"param"=1,
			"nonparam"=1,
			"semiparam"=input$numSemiVarianceWindow
		)
	)
}

DEFAULT_SPANS <- c(2,4,5,10,20,30,40,50,100,200)

spectral_estimation_ui <- function(id, title="Spectrum estimation parameters", methods=c("param", "nonparam", "semiparam")){
    ns <- NS(id)
	choiceNames <- c()
	choiceValues <- c()
	if("param" %in% methods){
		choiceNames <- c(choiceNames, "Parametric estimator")
		choiceValues <- c(choiceValues, "param")
	}
	if("nonparam" %in% methods){
		choiceNames <- c(choiceNames, "Nonparametric estimator")
		choiceValues <- c(choiceValues, "nonparam")
	}
	if("semiparam" %in% methods){
		choiceNames <- c(choiceNames, "Semi-parametric estimator")
		choiceValues <- c(choiceValues, "semiparam")
	}
	box(
		title=title, status = "warning", solidHeader = TRUE, width=12,
		collapsible = TRUE, collapsed=TRUE,
		radioButtons(
			ns("lstCategorySpecEstimation"),
			"Estimation method",
			choiceNames=choiceNames,
			choiceValues=choiceValues
		),
		if(!("param" %in% methods)) NULL else conditionalPanel(ns=ns, condition="input.lstCategorySpecEstimation == 'param' ", 
			helpText('This methods uses the Vector Autoregressive Model to estimated the spectrum:
				$$
					Y\\left(t\\right)=\\sum_{\\ell=1}^{p}\\Phi_{\\ell} X\\left(t-\\ell\\right)+\\varepsilon\\left(t\\right)
				$$
				$$
					\\varepsilon\\left(t\\right)
					\\sim
					\\mathcal{N}\\left(0,\\Sigma\\right)
				$$
			'),
			sliderInput(ns("txtParamVarOrder"), "VAR order:", 1, 10, 2, step=1),
			radioButtons(ns("lstParamEstimation"), "Estimation method", choiceNames=c('VAR/OLS', 'LASSLE (LASSO + LSE)', 'VAR/LASSO (BigVAR package)'), choiceValues=c("RawLSE", "LASSLE", "LassoBigVAR"))
		),
		if(!("nonparam" %in% methods)) NULL else conditionalPanel(ns=ns, condition="input.lstCategorySpecEstimation == 'nonparam' ",
			helpText('This methods uses smoothed versions of the Fast Fourier Transform.'),
			radioButtons(ns("lstNonparamEstimation"), "Estimation method", choiceNames=c( "Naive smoothed FFT", "PURE method"), choiceValues=c("FFT", "PURE")),
			selectInput(ns("lstNonparamWindowType"), "Window", c(
				"Gaussian"="gausswin",
				"Boxcar"="boxcar",
				"Triangular"="triang",
				"Hamming"="hamming",
				"Hanning"="hanning",
				"Blackman"="blackman",
				"Bartlett"="bartlett"
			), selected=NULL, multiple=FALSE, selectize=TRUE),
			#model <- non_parametric_spectrum_PURE(data, n_trials=n_trials, win_spans=win_spans, win=match.fun(win))
			#model <- non_parametric_spectrum_FFT(data, win_span=win_spans[1], win=match.fun(win))
			conditionalPanel(ns=ns, condition="input.lstNonparamEstimation == 'FFT' ",
				sliderInput(ns("numFFTSpans"), "Span:", 2, 100, 4, step=1),
			),
			conditionalPanel(ns=ns, condition="input.lstNonparamEstimation == 'PURE' ",
				sliderInput(ns("numPureTrials"), "Trials:", 2, 20, 4, step=1),
				selectInput(ns("lstPureSpans"), "Spans", DEFAULT_SPANS, selected=4, multiple=T, selectize=TRUE),
			)
		),
		#		model <- general_shrinkage_estimator(data, var_lag=var_order, C_T=variance_window_C_T, n_trials=n_trials, win_spans=win_spans, win=match.fun(win))
		if(!("semiparam" %in% methods)) NULL else conditionalPanel(ns=ns, condition="input.lstCategorySpecEstimation == 'semiparam' ",
			helpText('This methods mixed two components: a PURE FFT estimation ans a Vector Autoregressive Model to estimated the spectrum.'),
			sliderInput(ns("txtSemiVarOrder"), "VAR order:", 1, 10, 2, step=1),
			selectInput(ns("lstSemiWindowType"), "Window", c(
				"Gaussian"="gausswin",
				"Boxcar"="boxcar",
				"Triangular"="triang",
				"Hamming"="hamming",
				"Hanning"="hanning",
				"Blackman"="blackman",
				"Bartlett"="bartlett"
			), selected=NULL, multiple=FALSE, selectize=TRUE),
			sliderInput(ns("numSemiVarianceWindow"), "VarianceWindow $C_T$:", 10, 100, 11, step=1),
			sliderInput(ns("numSemiTrials"), "Trials:", 2, 20, 4, step=1),
			selectInput(ns("lstSemiSpans"), "Spans", DEFAULT_SPANS, selected=4, multiple=T, selectize=TRUE),
		)
	)
}


