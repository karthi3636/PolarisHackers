
score.sentiment = function(sentences, pos.words, neg.words,  .progress='none')
{
	require(plyr)
	require(stringr)
	p<<-as.integer(0)
	n<<-as.integer(0)
	nu<<-as.integer(0)
	# we got a vector of sentences. plyr will handle a list or a vector as an "l" for us
	# we want a simple array of scores back, so we use "l" + "a" + "ply" = laply:
	scores = laply(sentences, function(sentence, pos.words, neg.words, exc.words) {
	 
		# clean up sentences with R's regex-driven global substitute, gsub():
		sentence = gsub('[[:punct:]]', '', sentence)
		sentence = gsub('[[:cntrl:]]', '', sentence)
		sentence = gsub('\\d+', '', sentence)
		# and convert to lower case:
		sentence = tolower(sentence)

		# split into words. str_split is in the stringr package
		word.list = str_split(sentence, '\\s+')
		# sometimes a list() is one level of hierarchy too much
		words = unlist(word.list)

                # exclude stop words
              #  check <- match(words,exc.words)
               # exc.list <-!is.na(check)
              #  words <-words[!exc.list]

		# compare our words to the dictionaries of positive & negative terms
		pos.matches = match(words, pos.words)
		neg.matches = match(words, neg.words)
	
		# match() returns the position of the matched term or NA
		# we just want a TRUE/FALSE:
		pos.matches = !is.na(pos.matches)
		neg.matches = !is.na(neg.matches)

		# and conveniently enough, TRUE/FALSE will be treated as 1/0 by sum():
		score = sum(pos.matches) - sum(neg.matches)
	#	print(paste('score',score));
		
		if(score==0){
		value="Neutral"
		nu<<-sum(nu+1)
		}
			else if(score>0){
			value="Positive"
			p<<-sum(p+1)
			}
			else{
			  value="Negative"
			  n<<-sum(n+1)
			}
		#writeLines(value, "D:/karthi/outfile.txt")
		
		
		cat(value,file="D:/vz_sentiment/outfile.txt", sep = "\n",append=TRUE)
		return(score)
	}, pos.words, neg.words, .progress=.progress )
	
	print(paste('scores',scores));
	scores.df = data.frame(score=scores, text=sentences)
  
	file.copy("D:/vz_sentiment/outfile.txt","D:/vz_sentiment/PolarisHackers_Out.txt")
	file.remove("D:/vz_sentiment/outfile.txt")

	  
	#print(paste('Ne',n));
	#print(paste('posit',p));
	#print(paste('nu',nu));
	#writeLines(value, "D:/karthi/outfile.txt")
	vector = c()
	values = c(p,n,nu)
	for (i in 1:length(values))
	  colors <- c("Green","Red","Blue")
	  pie(values, main="Sentiment",col=colors,labels=c("Positive","Negative","Neutral"))
	
	legend(1.5, 0.5, c(p,n,nu), cex=0.8, 
	       fill=colors)
	
	## Pie with % centage
	sentiment_label <- round(values/sum(values) * 100, 1)
	
	# Concatenate a '%' char after each value
	sentiment_label <- paste(sentiment_label, "%", sep="")
	# Create a pie chart with defined heading and custom colors
	# and labels
	pie(values, main="Sentiment", col=colors, labels=sentiment_label,
	    cex=0.8)
	
	# Create a legend at the right   
	legend(1, 0.5, c("Positive","Negative","Neutral"), cex=0.8, 
	       fill=colors)
	
	
	return(scores.df)
}
