fileName="D:/vz_sentiment/HackathonInput.txt"
conn=file(fileName,open="r")
sample=readLines(conn)
close(conn)
analysis = score.sentiment(sample, pos, neg)
print("Report generated")