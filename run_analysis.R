merge_train_and_test <- function() {
	
	activity <- read.table("UCI HAR Dataset/activity_labels.txt")[,2]
	features <- read.table("UCI HAR Dataset/features.txt")[,2]
	
	d_test <- read_data(features, activity, FALSE)
	d_train <- read_data(features, activity, TRUE)
	
	d_train <- cbind(Set = "Train", d_train)
	d_test <- cbind(Set = "Test", d_test)
	
	rbind(d_test, d_train)
}

read_data <- function(features, activity, is.train = TRUE) {
	if(is.train) {
		subj <- read.table("UCI HAR Dataset/train/subject_train.txt"
			, header = FALSE, col.names = "SubjectId")
		ytr <- read.table("UCI HAR Dataset/train/y_train.txt"
			, header = FALSE)
		xtr <- read.table("UCI HAR Dataset/train/X_train.txt"
			, header = FALSE)
	}
	else {
		subj <- read.table("UCI HAR Dataset/test/subject_test.txt"
			, header = FALSE, col.names = "SubjectId")
		ytr <- read.table("UCI HAR Dataset/test/y_test.txt")
		xtr <- read.table("UCI HAR Dataset/test/X_test.txt"
			, header = FALSE)
	}
	
	names(xtr) <- features
	res <- cbind(subj, Activity = activity[ytr[,1]], xtr)
	
	return(res)
}

choose_mean_and_std <- function(names) {
	cols <- grepl("(mean\\(\\)|std\\(\\))",names(t), ignore.case = TRUE)
	cols[1:3] <- TRUE
	
	return(cols)
}

run_analysis <- function() {
	t <- merge_train_and_test()
	
	t1 <- t[,choose_mean_and_std(names(t))]
	
	t2 <- aggregate(t1, t1[, 2:3], mean)[,c(1:2,6:71)]
	
	write.table(t2, "tidy_dataset.txt", row.names = FALSE)
	
	return(t2)
}
