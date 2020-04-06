# cd -> ls automatically
c() {
	cd $1;
	ls -F;
}
alias cd="c"

qt() {
    TODAY=$(date +%Y-%m-%d)
    YEAR=$(date +%Y)
    MONTH=$(date +%m)
    YEAR_PATH="$HOME/Dropbox/qt/$YEAR"
    if [ ! -d "$YEAR_PATH" ];then
        mkdir $YEAR_PATH
    fi
    MONTH_PATH="$YEAR_PATH/$MONTH"
    if [ ! -d "$MONTH_PATH" ];then
        mkdir $MONTH_PATH
    fi
    FILE_PATH_PREFIX="$MONTH_PATH/${TODAY}"
    if ls $FILE_PATH_PREFIX* 1> /dev/null 2>&1; then
        FILE_PATH=$(ls $FILE_PATH_PREFIX*)
        echo "File already exists. Opening $FILE_PATH" 
        vim $FILE_PATH
    else
        read "passage?Not yet done! what is the passage?"
        echo $FILE_PATH_PREFIX
        echo $passage
        FILE_PATH="${FILE_PATH_PREFIX}_${passage}.txt"
        echo $FILE_PATH
        vim $FILE_PATH
    fi
}

qtl() {
    TODAY=$(date +%Y-%m-%d)
    YEAR=$(date +%Y)
    MONTH=$(date +%m)
    ls $HOME/Dropbox/qt/$YEAR/$MONTH/*
}
