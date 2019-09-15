# Copyright (c) 2019, Adrian Dusa
# All rights reserved.
# 
# Redistribution and use in source and binary forms, with or without
# modification, in whole or in part, are permitted provided that the
# following conditions are met:
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#     * Redistributions in binary form must reproduce the above copyright
#       notice, this list of conditions and the following disclaimer in the
#       documentation and/or other materials provided with the distribution.
#     * The names of its contributors may NOT be used to endorse or promote products
#       derived from this software without specific prior written permission.
# 
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS "AS IS" AND
# ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT LIMITED TO, THE IMPLIED
# WARRANTIES OF MERCHANTABILITY AND FITNESS FOR A PARTICULAR PURPOSE ARE
# DISCLAIMED. IN NO EVENT SHALL ADRIAN DUSA BE LIABLE FOR ANY
# DIRECT, INDIRECT, INCIDENTAL, SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES
# (INCLUDING, BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
# LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND
# ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE OF THIS
# SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.

`checkType` <- function(x, labels, xnumeric) {
    
    if (length(labels) > 0) {
        
        # possibly a categorical variable
        # but even numeric variables can have labels (for missing values)
        
        # verify if the number of unique values is (at most) equal to the number of labels
        uniquevals <- unique(x)
        
        if (length(uniquevals) <= length(labels)) {
            # surely a categorical variable
            return("cat")
        }
        else {
            
            # the number of unique values is greater than the number of labels
            # possibly a numeric variable (e.g. 1...10) with only two labels (for 1 and for 10)
            # or maybe a categorical variable for which not all values are labeled
            
            # unique values without labels
            nolabels <- setdiff(uniquevals, labels)
            
            # 5 and 8 are arbitrary numbers,
            # thinking of the smallest ordinal scale 1...7 that can be interpreted as "numeric"
            # and an even bigger numerical scale with labels, such as 1...10
            
            if (length(nolabels) < 5) {
                return("cat")
            }
            else if (length(nolabels) < 9) {
                return("numcat")
            }
            else {
                return("num")
            }
        }
    }
    
    return(ifelse(xnumeric, "num", "char"))
}
