############# script to clean ML/aged violet classification data and get subset of all valid clips for possible analysis
# then get final subset of videos for analysis that were classified by both C&S and expert

library(chron)

orig.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_2020-02-02_chimp_classifications_ML_only.csv",header=T,sep=",",fill=T,comment.char="",as.is=T,quote="")
nrow(orig.data)
#665917

########give better columns names
colnames(orig.data)<-c("classification.id","creation.date","clip.id","video.id","slices","start.time","duration","user.name","preview.set","annotation.number","presence","species","number.individuals","behavior","age","sex")

###########get rid of \" from basically everywhere
cols.to.gsub<-colnames(orig.data)
for(i in 1:length(cols.to.gsub)){
	orig.data[,cols.to.gsub[i]]<-gsub(orig.data[,cols.to.gsub[i]], pattern="\"", replace="")
}

ml.data<-orig.data
nrow(ml.data)
#665917

#this is the dataset that needs to be fixed then rejoined with the rest of ml.data
ml.to.fix<-ml.data[sort(c(which(ml.data$clip.id==""),which(ml.data$clip.id=="")-1)),]

#this is the ml data that's fine
ml.ok<-ml.data[setdiff(1:nrow(ml.data), c(which(ml.data$clip.id==""),which(ml.data$clip.id=="")-1)),]

#of the ml data to fix, we first separate the rows into the full rows and the mostly empty rows. they will be in the same order, so the first row of the full rows will correspond to the first row in the mostly empty rows dataset. that way, the unique code from the mostly empty rows dataset can be put into the sex column of the full rows, after the full rows have been shifted back to the left. this will only affect chimp classifications, but the shifting needs to be done for all of them. the resulting dataset, based on the full rows dataset, will be added back into ml.data.
ml.blanks<-ml.to.fix[seq(2, nrow(ml.to.fix), by=2),]

ml.full<-ml.to.fix[seq(1, nrow(ml.to.fix), by=2),]

all.equal(which(ml.blanks$classification.id!=""), which(ml.full$number.individuals=="chimpanzee"))
#TRUE, so the rows match up. Rows with chimps have rows with nonblank unique codes, which are the sexes of those chimps

#paste together the video.id and slices to get the proper full video id in the full data
ml.full$video.id<-paste(ml.full$video.id, ml.full$slices, sep="")
#all rows from columns duration to sex are put into columns start.time to age 
ml.full[,which(colnames(ml.full)=="start.time"):which(colnames(ml.full)=="age")]<-as.matrix(ml.full[,which(colnames(ml.full)=="duration"):which(colnames(ml.full)=="sex")])

#put the unique code column in the blank set into the sex column of the full set
ml.full$sex<-ml.blanks$classification.id

#combine ml.full with the rest of ml data
ml.data<-rbind(ml.ok, ml.full)
nrow(ml.data)
#659250

#################make separate folder and file ids
(start.time<-Sys.time())
ml.data$folder.id<-unlist(lapply(1:nrow(ml.data), function(x){

	sub.folder.id<-ml.data$video.id[x]
	split.parts<-unlist(strsplit(sub.folder.id, split="/"))
	return(split.parts[length(split.parts)-1])

}))
ml.data$file.id<-unlist(lapply(1:nrow(ml.data), function(x){

	sub.file.id<-ml.data$video.id[x]
	split.parts<-unlist(strsplit(sub.file.id, split="/"))
	sub.file.id<-split.parts[length(split.parts)]
	split.parts<-unlist(strsplit(sub.file.id, split=".", fixed=T))
	return(split.parts[1])

}))
end.time<-Sys.time()
end.time - start.time
#Time difference of 33 seconds

ml.data$folder.file.id<-paste(ml.data$folder.id, ml.data$file.id, sep="_")

#######clean some of ml.data's folders and files 
#clean folders
corr.folder.names<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML correct folder names.txt", sep="\t", header=T, stringsAsFactors=F)

#correct the folders in ml.data
for(i in 1:nrow(corr.folder.names)){

	ml.data$folder.id<-gsub(x=ml.data$folder.id, pattern=corr.folder.names$orig.folder.id[i], replacement=corr.folder.names$new.folder.id[i], fixed=T)
}

#clean files
corr.file.names<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML correct file names.txt", sep="\t", header=T, stringsAsFactors=F)

for(i in 1:nrow(corr.file.names)){

	ml.data$file.id<-gsub(x=ml.data$file.id, pattern=corr.file.names$orig.file.id[i], replacement=corr.file.names$new.file.id[i], fixed=T)
}

#repaste folder file id
ml.data$folder.file.id<-paste(ml.data$folder.id, ml.data$file.id, sep="_")

#remove "+" from the end of any folder ids
sum(substr(ml.data$folder.id, nchar(ml.data$folder.id), nchar(ml.data$folder.id))=="+")
#483063
ml.data$folder.id[substr(ml.data$folder.id, nchar(ml.data$folder.id), nchar(ml.data$folder.id))=="+"]<-substr(ml.data$folder.id[substr(ml.data$folder.id, nchar(ml.data$folder.id), nchar(ml.data$folder.id))=="+"], 1, nchar(ml.data$folder.id[substr(ml.data$folder.id, nchar(ml.data$folder.id), nchar(ml.data$folder.id))=="+"])-1)

sum(substr(ml.data$folder.id, nchar(ml.data$folder.id), nchar(ml.data$folder.id))=="+")
#0

ml.data$folder.file.id<-paste(ml.data$folder.id, ml.data$file.id, sep="_")
nrow(ml.data)
#659250
length(unique(ml.data$clip.id))
#94275
length(unique(ml.data$video.id))
#30231

###############get date/time into a usable format
#remove the UTC from creation.date
ml.data$r.date.time=substring(ml.data$creation.date, 1, 19)
ml.data$r.date.time=as.POSIXlt(strptime(ml.data$r.date.time, format="%Y-%m-%d %H:%M:%S", tz="UTC"))

#create separate columns for year, month and day
ml.data$year=ml.data$r.date.time$year+1900
ml.data$month=ml.data$r.date.time$mon+1
ml.data$day=ml.data$r.date.time$mday

#create time
ml.data$r.time=times(paste(ml.data$r.date.time$hour, ml.data$r.date.time$min, ml.data$r.date.time$sec, sep=":"))
ml.data$r.date=as.Date(paste(ml.data$year, ml.data$month, ml.data$day, sep="-"), format="%Y-%m-%d")

#get unique row numbers for later cleaning
ml.data$unique.row.number<-1:nrow(ml.data)

#make sure all date.times are ok
sum(is.na(ml.data$r.date.time))
#0


#####################sort data by user, clip and time
sort.data<-ml.data[order(ml.data$user.name, ml.data$clip.id, ml.data$r.date.time),]
sort.data$user.clip<-paste(sort.data$user.name, sort.data$clip.id, sep="_")

################create viewing bouts where all responses in a bout are from the same classification.id
changes<-as.numeric(sort.data$classification.id[-1]!=sort.data$classification.id[-nrow(sort.data)])

sort.data$instance.change<-c(1, changes)

sort.data$bout<-cumsum(sort.data$instance.change)

table(table(sort.data$bout))
#      1      2      3      4      5      6      7      8      9     10     11 
# 635074   6925   1512    545    315    137     78     36     21      7      6 

#     12     18 
#      3      1

#so there is one instance where a person said 18 lines' worth of species for a single clip at one time, etc

#######################remove duplicate bouts, where the same user/clip combination has multiple viewings, #meaning the same clip was shown more than once to a single user
num.bouts.per.user.clip<-aggregate(sort.data$bout, list(sort.data$user.clip), function(x){length(unique(x))})
table(num.bouts.per.user.clip$x)
#      1      2      3      4      5      6      7      8      9     10     11 
# 618237  10441    949    144     40     32     20     11      6     10      6 

#     12     13     15     16     17     19     21     32     33     41     50 
#      3      2      1     13      1      1      1      1      1      1      1 

#     64     97    106    199    314 
#      1      1      1      1      1

user.clips.with.multi.bouts<-num.bouts.per.user.clip[num.bouts.per.user.clip$x>1,]

multi.bout.data<-sort.data[sort.data$user.clip %in% user.clips.with.multi.bouts$Group.1,]

#find the bouts to delete, for each user.clip, remove all bouts except the first
dup.bouts<-unlist(lapply(unique(multi.bout.data$user.clip), function(clip.arg){

	sub.bouts<-unique(multi.bout.data$bout[multi.bout.data$user.clip==clip.arg])
	return(sub.bouts[-1])

}))

nrow(sort.data)
#659250
length(unique(sort.data$clip.id))
#94275
length(unique(sort.data$video.id))
#30231
sort.data<-sort.data[!sort.data$bout %in% dup.bouts,]
nrow(sort.data)
#644231
length(unique(sort.data$clip.id))
#94275
length(unique(sort.data$video.id))
#30231

#check to make sure all user.clips only have 1 bout now
table(aggregate(sort.data$bout, list(sort.data$user.clip), function(x){length(unique(x))})$x)
#      1 
# 629927 

bout.table<-table(sort.data$bout)
table(bout.table)
#      1      2      3      4      5      6      7      8      9     10     11     12     18 
# 620549   6759   1492    530    311    137     76     35     21      7      6      3      1 


###remove any species=blank rows where there are blanks and species present in same bout
start.time<-Sys.time()
blank.row.numbers.to.remove<-unlist(lapply(as.numeric(names(bout.table[bout.table>1])), function(bout.arg){
    sub.data<-sort.data[sort.data$bout==bout.arg,]
    if(sum(sub.data$species!="")>0 & sum(sub.data$species=="")>0){
        return(sub.data$unique.row.number[sub.data$species==""])
    } else {
        return(NA)
    }
}))
end.time<-Sys.time()
end.time - start.time
#8.5 minutes
length(blank.row.numbers.to.remove)
#9378
blank.row.numbers.to.remove<-blank.row.numbers.to.remove[!is.na(blank.row.numbers.to.remove)]
length(blank.row.numbers.to.remove)
#0, no instances of blanks and species together in same bout


############### aggregate repeated species into one line that contains the total number of individuals in that species summed across all lines for that species
# we don't need the behaviors/ages/sexes for anything in this analysis so don't change it
# to get final number.individuals, add up the numbers in the number.individuals column, with 5+ being changed to 5 as a conservative measure, which we'll have to do in the count analysis anyway
sort.data$bout.species<-paste(sort.data$bout, sort.data$species, sep="_")
bout.species.table<-table(sort.data$bout.species)
table(bout.species.table)
#      1      2      3      4      5      6      7      8      9     10     11     12 
# 627020   4063   1294    494    284    115     75     32     21      9      3      2 

#turn r.date, r.time, and r.date.time into characters because otherwise it gets messy
sort.data$r.date.time<-as.character(sort.data$r.date.time)
sort.data$r.time<-as.character(sort.data$r.time)
sort.data$r.date<-as.character(sort.data$r.date)

nonrepeat.data<-sort.data[sort.data$bout.species %in% names(bout.species.table[bout.species.table==1]),]
nrow(nonrepeat.data)
#627020

repeated.data<-sort.data[sort.data$bout.species %in% names(bout.species.table[bout.species.table>1]),]
nrow(repeated.data)
#17211

bouts.to.agg<-names(bout.species.table[bout.species.table>1])

agg.repeat.data<-data.frame(matrix(NA, ncol=ncol(sort.data), nrow=length(bouts.to.agg)))
colnames(agg.repeat.data)<-colnames(sort.data)

start.time<-Sys.time()
for(i in 1:length(bouts.to.agg)){
    cat(i, "\n")
    sub.data<-repeated.data[repeated.data$bout.species==bouts.to.agg[i],]
    agg.repeat.data[i,]<-as.character(sub.data[1,])
    agg.counts<-gsub(sub.data$number.individuals, pattern="5+", replacement="5", fixed=T)
    agg.repeat.data$number.individuals[i]<-sum(as.numeric(agg.counts))
    flush.console()
}
end.time<-Sys.time()
end.time - start.time
#53 seconds

#join the newly aggregated data with the original data that didn't need aggregation
sort.data<-rbind(nonrepeat.data, agg.repeat.data)
nrow(sort.data)
#633412
length(unique(sort.data$clip.id))
#94275
length(unique(sort.data$video.id))
#30231

bout.table<-table(sort.data$bout)
table(bout.table)
#      1      2      3      4      5      8     10 
# 626559   3271     88      6      1      1      1 

#so there are 3368 multispecies classifications

############### determine how many species C&S thinks are in each clip
#### use median number of species like in snapshot paper
# all blanks will be a species number of 0
# some blanks and some species will have blanks removed then median species calculated
num.species.df<-data.frame(clip.id=unique(sort.data$clip.id), median.num.species=NA)
total.rows<-length(unique(sort.data$clip.id))

start.time<-Sys.time()
for(i in 1:nrow(num.species.df)){
	cat(i, round(i/total.rows*100, 2), "% \n")
	sub.data<-sort.data[sort.data$clip.id==num.species.df$clip.id[i],]
	sub.num.users.nonblank<-length(unique(sub.data$user.name[sub.data$species!=""]))

	if(sub.num.users.nonblank==0){
		num.species.df$median.num.species[i]<-0
	} else {
		sub.data<-sub.data[sub.data$species!="",]
		num.species.df$median.num.species[i]<-median(table(sub.data$user.name))
	}

	flush.console()
}
end.time<-Sys.time()
end.time - start.time
#1.3 hours

table(num.species.df$median.num.species)
#     0     1   1.5     2   2.5     3     4     8 
# 54358 39547    68   296     1     3     1     1 

#so 370 clips will be removed due to being considered multispecies

############## continue with only clips where C&S said 0 or 1 species is present and remove multispecies classifications from those clips
single.sp.clip.info<-num.species.df[num.species.df$median.num.species %in% c(0,1),]
nrow(single.sp.clip.info)
#93905
single.sp.data<-sort.data[sort.data$clip.id %in% single.sp.clip.info$clip.id,]
single.sp.data<-single.sp.data[single.sp.data$bout %in% as.numeric(names(bout.table[bout.table==1])),]
nrow(single.sp.data)
#624437
length(unique(single.sp.data$clip.id))
#93905
length(unique(single.sp.data$video.id))
#30191


################################
# now determine which videos are 4/4 valid
# such videos have 4 clips present where each clip either has:
# 4+ nonblank classifications
# or 3 blank classifications with 0 nonblank classifications (or more blanks, which are retirement limit bugs but they still were retired fully blank)

####### loop through and get number blanks and number nonblanks per clip
single.sp.clip.info$num.blanks<-NA
single.sp.clip.info$num.nonblanks<-NA
start.time<-Sys.time()
for(i in 1:nrow(single.sp.clip.info)){
	cat(i, round(i/93905*100, 2), "% \n")
	sub.data<-single.sp.data[single.sp.data$clip.id==single.sp.clip.info$clip.id[i],]
	single.sp.clip.info$num.blanks[i]<-sum(sub.data$species=="")
	single.sp.clip.info$num.nonblanks[i]<-sum(sub.data$species!="")
	flush.console()
}
end.time<-Sys.time()
end.time - start.time
#1.4 hours

valid.clips<-single.sp.clip.info[single.sp.clip.info$num.nonblanks>=4 | (single.sp.clip.info$num.nonblanks==0 & single.sp.clip.info$num.blanks>=3),]
#add in folder.file.id
valid.clips$folder.file.id<-single.sp.data$folder.file.id[match(valid.clips$clip.id, single.sp.data$clip.id)]
valid.clips.table<-table(valid.clips$folder.file.id)
table(valid.clips.table)
#     1     2     3     4 
#  4605  7266  4514 13572

valid.data<-single.sp.data[single.sp.data$folder.file.id %in% names(valid.clips.table[valid.clips.table==4]),]
nrow(valid.data)
#351128
length(unique(valid.data$clip.id))
#54288
length(unique(valid.data$video.id))
#13572

########################################
# now bring in expert data
expert.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_expert_data_to_start_with_for_github.txt", sep="\t", header=T)
nrow(expert.data)
#29240

############ then expert data drops videos that do not correspond to 4/4 valid videos
expert.data<-expert.data[expert.data$folder.file.id %in% unique(valid.data$folder.file.id),]
nrow(expert.data)
#10874
length(unique(expert.data$folder.file.id))
#10704

############# then expert data drops videos that are multispecies
multisp.expert.videos<-table(expert.data$folder.file.id)
table(multisp.expert.videos)
#     1     2     3 
# 10541   156     7 

#so drop 163 videos
expert.data<-expert.data[expert.data$folder.file.id %in% names(multisp.expert.videos[multisp.expert.videos==1]),]
nrow(expert.data)
#10541

############# then expert data drops videos that are duiker sp
table(expert.data$vernacular_name)
expert.data<-expert.data[expert.data$vernacular_name!="Duiker_sp",]
nrow(expert.data)
#10454

######### then expert data drops videos that are unidentifiable
expert.data<-expert.data[expert.data$vernacular_name!="UID",]
nrow(expert.data)
#10412


###### now get classification data for just these 10412 videos
class.data<-valid.data[valid.data$folder.file.id %in% expert.data$folder.file.id,]
nrow(class.data)
#264267
length(unique(class.data$clip.id))
#41648
length(unique(class.data$video.id))
#10412


#####################################
# now make all species names match for original, umbrella duiker, and umbrella designations

trans.data<-read.table("D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML translation data umbrella species.txt", sep="\t", header=T, quote="")
nrow(trans.data)
#65
#use "blank" instead of ""
class.data$species[class.data$species==""]<-"blank"
trans.data$vernacular_name[is.na(trans.data$vernacular_name)]<-"blank"
trans.data$C.S.classification.category[trans.data$C.S.classification.category==""]<-"blank"
trans.data$umbrella.duiker.species[trans.data$umbrella.duiker.species==""]<-"blank"
trans.data$umbrella.species[trans.data$umbrella.species==""]<-"blank"

#class data
class.data$umbrella.duiker.species<-trans.data$umbrella.duiker.species[match(class.data$species, trans.data$C.S.classification.category)]
class.data$umbrella.species<-trans.data$umbrella.species[match(class.data$species, trans.data$C.S.classification.category)]

#expert data
expert.data$species<-trans.data$C.S.classification.category[match(expert.data$vernacular_name, trans.data$vernacular_name)]
expert.data$umbrella.duiker.species<-trans.data$umbrella.duiker.species[match(expert.data$vernacular_name, trans.data$vernacular_name)]
expert.data$umbrella.species<-trans.data$umbrella.species[match(expert.data$vernacular_name, trans.data$vernacular_name)]



write.table(expert.data, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_expert_data_for_analysis.txt", sep="\t", row.names=F, col.names=T)
write.table(class.data, "D:/Desktop/iDiv_ChimpAndSee/Paper analyses/Rerun 2021/1_ML_class_data_for_analysis.txt", sep="\t", row.names=F, col.names=T)




