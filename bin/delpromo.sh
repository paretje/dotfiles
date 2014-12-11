#!/bin/bash
wget -q -O /tmp/delpromo.pdf $(./getdelpromourl.sh)

# Source: http://www.zedwood.com/article/bash-send-mail-with-an-attachment
function fappend {
    echo "$2">>$1;
}
 
# CHANGE THESE
TOEMAIL="kevindeprey@Online-Urbanus.be";
FREMAIL="info@Online-Urbanus.be";
SUBJECT="Delhaize Promo's";
ATTACHMENT="/tmp/delpromo.pdf"
MIMETYPE="application/pdf"
 
# DON'T CHANGE ANYTHING BELOW
TMP="/tmp/delpromo_"$RANDOM;
BOUNDARY=`date +%s|md5sum`
BOUNDARY=${BOUNDARY:0:32}
FILENAME=`basename $ATTACHMENT`
 
rm -rf $TMP;
cat $ATTACHMENT | base64> $TMP;
sed -i -e '1,1d' -e '$d' $TMP;#removes first & last lines from $TMP
DATA=`cat $TMP`
 
rm -rf $TMP;
fappend $TMP "From: $FREMAIL";
fappend $TMP "To: $TOEMAIL";
fappend $TMP "Reply-To: $FREMAIL";
fappend $TMP "Subject: $SUBJECT";
fappend $TMP "Content-Type: multipart/mixed; boundary=\""$BOUNDARY"\"";
fappend $TMP "";
fappend $TMP "This is a MIME formatted message.  If you see this text it means that your";
fappend $TMP "email software does not support MIME formatted messages.";
fappend $TMP "";
fappend $TMP "--$BOUNDARY";
fappend $TMP "Content-Type: text/plain; charset=UTF-8; format=flowed";
fappend $TMP "Content-Transfer-Encoding: quoted-printable";
fappend $TMP "Content-Disposition: inline";
fappend $TMP "";
fappend $TMP "--$BOUNDARY";
fappend $TMP "Content-Type: $MIMETYPE; name=\"$FILENAME\"";
fappend $TMP "Content-Transfer-Encoding: base64";
fappend $TMP "Content-Disposition: attachment; filename=\"$FILENAME\";";
fappend $TMP "";
fappend $TMP "$DATA";
fappend $TMP "";
fappend $TMP "";
fappend $TMP "--$BOUNDARY--";
fappend $TMP "";
fappend $TMP "";
cat $TMP | /usr/lib/sendmail -t;
rm $TMP;

rm /tmp/delpromo.pdf
