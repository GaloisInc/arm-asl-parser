#!/bin/sh

sed -e 's/type1 PARTIDtype/type PARTIDtype/;s/type1 PMGtype/type PMGtype/;s/type1 MPAMinfo/type MPAMinfo/' $1
