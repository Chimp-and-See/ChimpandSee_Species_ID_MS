#find consensus species per clip and video for original, umbrella duiker, and umbrella designations

#find consensus species per clip (the consensus species is the single species with a proportion >=0.5 of the responses. If there were 2 species that were 0.5, or if there was one species that was most frequenet but at a total prop less than 0.5, then there it didn't reach consensus. the proportion agreement for each clip is the proportion of species that match the expert species, regardless of what the consensus species was.)
#then find video consensus (consider only clips that were not retired as blank. if all species clips were nonconsensus, then the video was nonconsensus. otherwise, consider only clip(s) that have the highest consensus proportion. if the those remaining clips have multiple consensus species represented, the video does not reach consensus. if it's just one species, then the video reaches consensus on that species. the proportion agreement for the video is the average of the clips that were considered for the consensus species determination)

#############################################
#                 original                  #
#############################################

#bring in expert data
expert.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_expert_data_for_analysis.txt", sep="\t", header=T)
nrow(expert.data)
#10412

########################find consensus species per clip
class.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_class_data_for_analysis.txt", sep="\t", header=T)
nrow(class.data)
#264267
class.data<-class.data[order(class.data$clip.id),]

#create summ.data to house summaries for each clip
summ.data<-data.frame(clip.id=sort(unique(class.data$clip.id)), ffi=NA, expert.sp=NA, consensus.sp=NA, consensus.prop=NA, prop.agreement=NA, num.blanks=NA, num.nonblanks=NA)
summ.data$ffi<-class.data$folder.file.id[match(summ.data$clip.id, class.data$clip.id)]
summ.data$expert.sp<-expert.data$species[match(summ.data$ffi, expert.data$folder.file.id)]
nrow(summ.data)
#41648

consensus.threshold<-0.5

#summarize the consensus per clip and proportion agreement to expert.id
start.time<-Sys.time()
for(i in 1:nrow(summ.data)){
    cat(i, round(i/41648*100, 2), "% \n")
	sub.class<-class.data[class.data$clip.id==summ.data$clip.id[i],]
	sub.expert.sp<-expert.data$species[expert.data$folder.file.id==summ.data$ffi[i]]

	num.blanks<-sum(sub.class$species=="blank")
	num.nonblanks<-sum(sub.class$species!="blank")

	summ.data$num.blanks[i]<-num.blanks
	summ.data$num.nonblanks[i]<-num.nonblanks
	

	#if the clip is only blanks and at least 3, then species is blank
	if(num.blanks>=3 & num.nonblanks==0){
		summ.data$consensus.sp[i]<-"blank"
		summ.data$consensus.prop[i]<-1
		summ.data$prop.agreement[i]<-mean(sub.class$species==sub.expert.sp)
		
	#otherwise find a consensus (all clips should meet either condition, this is for peace of mind)
	} else if(num.nonblanks>=4){
		sub.class<-sub.class[sub.class$species!="blank",]	
		
		sp.table<-table(sub.class$species)
		max.prop<-max(sp.table)/sum(sp.table)
		max.sp<-names(sp.table[sp.table==max(sp.table)])
		#if only one species has the maximum proportion of responses, and the prop is at least 0.5, then clip reaches consensus on that species
		if(length(max.sp)==1 & max.prop>=consensus.threshold){
			summ.data$consensus.sp[i]<-max.sp
			summ.data$consensus.prop[i]<-max.prop
			summ.data$prop.agreement[i]<-mean(sub.class$species==sub.expert.sp)
		#otherwise doesn't reach consensus
		} else {
			summ.data$consensus.sp[i]<-"no consensus"
			summ.data$consensus.prop[i]<-NA
			summ.data$prop.agreement[i]<-NA
		}
	
	}
}
end.time<-Sys.time()
end.time - start.time
#19 mins

sum(is.na(summ.data$consensus.sp))

write.table(summ.data, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_clip_original.txt", sep="\t", row.names=F, col.names=T)


########################find consensus species per video
clip.cons.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_clip_original.txt", sep="\t", header=T, stringsAsFactor=F)
nrow(clip.cons.data)
#41648
length(unique(clip.cons.data$ffi))
#10412 videos 

clip.cons.data<-clip.cons.data[order(clip.cons.data$ffi),]

video.cons<-data.frame(ffi=unique(clip.cons.data$ffi), expert.sp=NA, consensus.sp=NA, consensus.prop=NA, prop.agreement=NA, consensus.type=NA)
video.cons$expert.sp<-clip.cons.data$expert.sp[match(video.cons$ffi, clip.cons.data$ffi)]
nrow(video.cons)
#10412

start.time<-Sys.time()
for(i in 1:nrow(video.cons)){
    cat(i, round(i/10412*100, 2), "% \n")
	sub.cons<-clip.cons.data[clip.cons.data$ffi==video.cons$ffi[i],]
	
	#if there are only blanks, then video is blank
	if(sum(sub.cons$consensus.sp=="blank")==nrow(sub.cons)){
		video.cons$consensus.type[i]<-"Consensus blank"

	#if there are only nonblank nonconsensus clips, don't continue, video is non consensus
	} else if(sum(sub.cons$consensus.sp=="no consensus")==sum(sub.cons$consensus.sp!="blank")){
		video.cons$consensus.type[i]<-"All nonblank clips are nonconsensus"
	
	} else {
		#remove any nonconsensus species and blanks
		sub.cons<-sub.cons[!sub.cons$consensus.sp %in% c("no consensus","blank"),]
		
		#keep only rows that have the highest consensus prop
		sub.cons<-sub.cons[sub.cons$consensus.prop==max(sub.cons$consensus.prop),]
		
		#if there are multiple species present, then no consensus
		if(length(unique(sub.cons$consensus.sp))>1){
			video.cons$consensus.type[i]<-"Conflicting consensus species"
		
		#if there's just one species present, then video reaches consensus on that species, at that max proportion, with the average prop.agreement
		} else {
			video.cons$consensus.type[i]<-"Consensus reached"
			video.cons$consensus.sp[i]<-sub.cons$consensus.sp[1]
			video.cons$consensus.prop[i]<-sub.cons$consensus.prop[1]
			video.cons$prop.agreement[i]<-mean(sub.cons$prop.agreement)

		}
	}
}
end.time<-Sys.time()
end.time - start.time
#15 seconds

table(video.cons$consensus.type)
# All nonblank clips are nonconsensus       Conflicting consensus species 
#                                 540                                  21 
#                     Consensus blank                   Consensus reached 
#                                3168                                6683
							   

write.table(video.cons, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_video_original.txt", sep="\t", row.names=F, col.names=T)





#############################################
#              umbrella duiker              #
#############################################

#bring in expert data
expert.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_expert_data_for_analysis.txt", sep="\t", header=T)
nrow(expert.data)
#10412

########################find consensus species per clip
class.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_class_data_for_analysis.txt", sep="\t", header=T)
nrow(class.data)
#264267
class.data<-class.data[order(class.data$clip.id),]

#create summ.data to house summaries for each clip
summ.data.umbrella.duiker<-data.frame(clip.id=sort(unique(class.data$clip.id)), ffi=NA, expert.sp=NA, consensus.sp=NA, consensus.prop=NA, prop.agreement=NA, num.blanks=NA, num.nonblanks=NA)
summ.data.umbrella.duiker$ffi<-class.data$folder.file.id[match(summ.data.umbrella.duiker$clip.id, class.data$clip.id)]
summ.data.umbrella.duiker$expert.sp<-expert.data$umbrella.duiker.species[match(summ.data.umbrella.duiker$ffi, expert.data$folder.file.id)]
nrow(summ.data.umbrella.duiker)
#41648

consensus.threshold<-0.5

#summarize the consensus per clip and proportion agreement to expert.id
start.time<-Sys.time()
for(i in 1:nrow(summ.data.umbrella.duiker)){
    cat(i, round(i/41648*100, 2), "% \n")
	sub.class<-class.data[class.data$clip.id==summ.data.umbrella.duiker$clip.id[i],]
	sub.expert.sp<-expert.data$umbrella.duiker.species[expert.data$folder.file.id==summ.data.umbrella.duiker$ffi[i]]

	num.blanks<-sum(sub.class$umbrella.duiker.species=="blank")
	num.nonblanks<-sum(sub.class$umbrella.duiker.species!="blank")

	summ.data.umbrella.duiker$num.blanks[i]<-num.blanks
	summ.data.umbrella.duiker$num.nonblanks[i]<-num.nonblanks
	
	#if the clip is only blanks and at least 3, then species is blank
	if(num.blanks>=3 & num.nonblanks==0){
		summ.data.umbrella.duiker$consensus.sp[i]<-"blank"
		summ.data.umbrella.duiker$consensus.prop[i]<-1
		summ.data.umbrella.duiker$prop.agreement[i]<-mean(sub.class$umbrella.duiker.species==sub.expert.sp)
		
	#otherwise find a consensus (all clips should meet either condition, this is for peace of mind)
	} else if(num.nonblanks>=4){
		sub.class<-sub.class[sub.class$umbrella.duiker.species!="blank",]	
		
		sp.table<-table(sub.class$umbrella.duiker.species)
		max.prop<-max(sp.table)/sum(sp.table)
		max.sp<-names(sp.table[sp.table==max(sp.table)])
		#if only one species has the maximum proportion of responses, and the prop is at least 0.5, then clip reaches consensus on that species
		if(length(max.sp)==1 & max.prop>=consensus.threshold){
			summ.data.umbrella.duiker$consensus.sp[i]<-max.sp
			summ.data.umbrella.duiker$consensus.prop[i]<-max.prop
			summ.data.umbrella.duiker$prop.agreement[i]<-mean(sub.class$umbrella.duiker.species==sub.expert.sp)
		#otherwise doesn't reach consensus
		} else {
			summ.data.umbrella.duiker$consensus.sp[i]<-"no consensus"
			summ.data.umbrella.duiker$consensus.prop[i]<-NA
			summ.data.umbrella.duiker$prop.agreement[i]<-NA
		}
	
	}
}
end.time<-Sys.time()
end.time - start.time
#19 mins

sum(is.na(summ.data.umbrella.duiker$consensus.sp))

write.table(summ.data.umbrella.duiker, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_clip_umbrella_duiker.txt", sep="\t", row.names=F, col.names=T)


########################find consensus species per video
clip.cons.data.umbrella.duiker<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_clip_umbrella_duiker.txt", sep="\t", header=T, stringsAsFactor=F)
nrow(clip.cons.data.umbrella.duiker)
#41648
length(unique(clip.cons.data.umbrella.duiker$ffi))
#10412 videos 

clip.cons.data.umbrella.duiker<-clip.cons.data.umbrella.duiker[order(clip.cons.data.umbrella.duiker$ffi),]

video.cons.umbrella.duiker<-data.frame(ffi=unique(clip.cons.data.umbrella.duiker$ffi), expert.sp=NA, consensus.sp=NA, consensus.prop=NA, prop.agreement=NA, consensus.type=NA)
video.cons.umbrella.duiker$expert.sp<-clip.cons.data.umbrella.duiker$expert.sp[match(video.cons.umbrella.duiker$ffi, clip.cons.data.umbrella.duiker$ffi)]
nrow(video.cons.umbrella.duiker)
#10412

start.time<-Sys.time()
for(i in 1:nrow(video.cons.umbrella.duiker)){
    cat(i, round(i/10412*100, 2), "% \n")
	sub.cons<-clip.cons.data.umbrella.duiker[clip.cons.data.umbrella.duiker$ffi==video.cons.umbrella.duiker$ffi[i],]
	
	#if there are only blanks, then video is blank
	if(sum(sub.cons$consensus.sp=="blank")==nrow(sub.cons)){
		video.cons.umbrella.duiker$consensus.type[i]<-"Consensus blank"

	#if there are only nonblank nonconsensus clips, don't continue, video is non consensus
	} else if(sum(sub.cons$consensus.sp=="no consensus")==sum(sub.cons$consensus.sp!="blank")){
		video.cons.umbrella.duiker$consensus.type[i]<-"All nonblank clips are nonconsensus"
	
	} else {
		#remove any nonconsensus species and blanks
		sub.cons<-sub.cons[!sub.cons$consensus.sp %in% c("no consensus","blank"),]
		
		#keep only rows that have the highest consensus prop
		sub.cons<-sub.cons[sub.cons$consensus.prop==max(sub.cons$consensus.prop),]
		
		#if there are multiple species present, then no consensus
		if(length(unique(sub.cons$consensus.sp))>1){
			video.cons.umbrella.duiker$consensus.type[i]<-"Conflicting consensus species"
		
		#if there's just one species present, then video reaches consensus on that species, at that max proportion, with the average prop.agreement
		} else {
			video.cons.umbrella.duiker$consensus.type[i]<-"Consensus reached"
			video.cons.umbrella.duiker$consensus.sp[i]<-sub.cons$consensus.sp[1]
			video.cons.umbrella.duiker$consensus.prop[i]<-sub.cons$consensus.prop[1]
			video.cons.umbrella.duiker$prop.agreement[i]<-mean(sub.cons$prop.agreement)

		}
	}
}
end.time<-Sys.time()
end.time - start.time
#15 seconds

table(video.cons.umbrella.duiker$consensus.type)
# All nonblank clips are nonconsensus       Conflicting consensus species 
#                                  99                                  19 
#                     Consensus blank                   Consensus reached 
#                                3168                                7126
							   
write.table(video.cons.umbrella.duiker, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_video_umbrella_duiker.txt", sep="\t", row.names=F, col.names=T)





#############################################
#                  umbrella                 #
#############################################

#bring in expert data
expert.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_expert_data_for_analysis.txt", sep="\t", header=T)
nrow(expert.data)
#10412

########################find consensus species per clip
class.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_class_data_for_analysis.txt", sep="\t", header=T)
nrow(class.data)
#264267
class.data<-class.data[order(class.data$clip.id),]

#create summ.data to house summaries for each clip
summ.data.umbrella<-data.frame(clip.id=sort(unique(class.data$clip.id)), ffi=NA, expert.sp=NA, consensus.sp=NA, consensus.prop=NA, prop.agreement=NA, num.blanks=NA, num.nonblanks=NA)
summ.data.umbrella$ffi<-class.data$folder.file.id[match(summ.data.umbrella$clip.id, class.data$clip.id)]
summ.data.umbrella$expert.sp<-expert.data$umbrella.species[match(summ.data.umbrella$ffi, expert.data$folder.file.id)]
nrow(summ.data.umbrella)
#41648

consensus.threshold<-0.5

#summarize the consensus per clip and proportion agreement to expert.id
start.time<-Sys.time()
for(i in 1:nrow(summ.data.umbrella)){
    cat(i, round(i/41648*100, 2), "% \n")
	sub.class<-class.data[class.data$clip.id==summ.data.umbrella$clip.id[i],]
	sub.expert.sp<-expert.data$umbrella.species[expert.data$folder.file.id==summ.data.umbrella$ffi[i]]

	num.blanks<-sum(sub.class$umbrella.species=="blank")
	num.nonblanks<-sum(sub.class$umbrella.species!="blank")

	summ.data.umbrella$num.blanks[i]<-num.blanks
	summ.data.umbrella$num.nonblanks[i]<-num.nonblanks
	
	#if the clip is only blanks and at least 3, then species is blank
	if(num.blanks>=3 & num.nonblanks==0){
		summ.data.umbrella$consensus.sp[i]<-"blank"
		summ.data.umbrella$consensus.prop[i]<-1
		summ.data.umbrella$prop.agreement[i]<-mean(sub.class$umbrella.species==sub.expert.sp)
		
	#otherwise find a consensus (all clips should meet either condition, this is for peace of mind)
	} else if(num.nonblanks>=4){
		sub.class<-sub.class[sub.class$umbrella.species!="blank",]	
		
		sp.table<-table(sub.class$umbrella.species)
		max.prop<-max(sp.table)/sum(sp.table)
		max.sp<-names(sp.table[sp.table==max(sp.table)])
		#if only one species has the maximum proportion of responses, and the prop is at least 0.5, then clip reaches consensus on that species
		if(length(max.sp)==1 & max.prop>=consensus.threshold){
			summ.data.umbrella$consensus.sp[i]<-max.sp
			summ.data.umbrella$consensus.prop[i]<-max.prop
			summ.data.umbrella$prop.agreement[i]<-mean(sub.class$umbrella.species==sub.expert.sp)
		#otherwise doesn't reach consensus
		} else {
			summ.data.umbrella$consensus.sp[i]<-"no consensus"
			summ.data.umbrella$consensus.prop[i]<-NA
			summ.data.umbrella$prop.agreement[i]<-NA
		}
	
	}
}
end.time<-Sys.time()
end.time - start.time
#13 mins

sum(is.na(summ.data.umbrella$consensus.sp))

write.table(summ.data.umbrella, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_clip_umbrella.txt", sep="\t", row.names=F, col.names=T)


########################find consensus species per video
clip.cons.data.umbrella<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_clip_umbrella.txt", sep="\t", header=T, stringsAsFactor=F)
nrow(clip.cons.data.umbrella)
#41648
length(unique(clip.cons.data.umbrella$ffi))
#10412 videos 

clip.cons.data.umbrella<-clip.cons.data.umbrella[order(clip.cons.data.umbrella$ffi),]

video.cons.umbrella<-data.frame(ffi=unique(clip.cons.data.umbrella$ffi), expert.sp=NA, consensus.sp=NA, consensus.prop=NA, prop.agreement=NA, consensus.type=NA)
video.cons.umbrella$expert.sp<-clip.cons.data.umbrella$expert.sp[match(video.cons.umbrella$ffi, clip.cons.data.umbrella$ffi)]
nrow(video.cons.umbrella)
#10412

start.time<-Sys.time()
for(i in 1:nrow(video.cons.umbrella)){
    cat(i, round(i/10412*100, 2), "% \n")
	sub.cons<-clip.cons.data.umbrella[clip.cons.data.umbrella$ffi==video.cons.umbrella$ffi[i],]
	
	#if there are only blanks, then video is blank
	if(sum(sub.cons$consensus.sp=="blank")==nrow(sub.cons)){
		video.cons.umbrella$consensus.type[i]<-"Consensus blank"

	#if there are only nonblank nonconsensus clips, don't continue, video is non consensus
	} else if(sum(sub.cons$consensus.sp=="no consensus")==sum(sub.cons$consensus.sp!="blank")){
		video.cons.umbrella$consensus.type[i]<-"All nonblank clips are nonconsensus"
	
	} else {
		#remove any nonconsensus species and blanks
		sub.cons<-sub.cons[!sub.cons$consensus.sp %in% c("no consensus","blank"),]
		
		#keep only rows that have the highest consensus prop
		sub.cons<-sub.cons[sub.cons$consensus.prop==max(sub.cons$consensus.prop),]
		
		#if there are multiple species present, then no consensus
		if(length(unique(sub.cons$consensus.sp))>1){
			video.cons.umbrella$consensus.type[i]<-"Conflicting consensus species"
		
		#if there's just one species present, then video reaches consensus on that species, at that max proportion, with the average prop.agreement
		} else {
			video.cons.umbrella$consensus.type[i]<-"Consensus reached"
			video.cons.umbrella$consensus.sp[i]<-sub.cons$consensus.sp[1]
			video.cons.umbrella$consensus.prop[i]<-sub.cons$consensus.prop[1]
			video.cons.umbrella$prop.agreement[i]<-mean(sub.cons$prop.agreement)

		}
	}
}
end.time<-Sys.time()
end.time - start.time
#15 seconds

table(video.cons.umbrella$consensus.type)
# All nonblank clips are nonconsensus       Conflicting consensus species 
#                                  85                                  18 
#                     Consensus blank                   Consensus reached 
#                                3168                                7141 
							   
write.table(video.cons.umbrella, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_consensus_per_video_umbrella.txt", sep="\t", row.names=F, col.names=T)


# save.image("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_find_consensus_per_clip_and_video.RData")
load("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/2_find_consensus_per_clip_and_video.RData")

