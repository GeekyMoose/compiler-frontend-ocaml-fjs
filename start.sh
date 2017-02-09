#!/bin/bash
#
#

# ------------------------------------------------------------------------------
# CONSTANTS - VARIABLES
# ------------------------------------------------------------------------------
LEXER_FILE='lexer.mll'
LEXER_GENERATED='generated-lexer.ml'
LEXER_COMPILED='compiled-lexer'


# ------------------------------------------------------------------------------
# FUNCTIONS
# ------------------------------------------------------------------------------

#
# Check whether last function successfully ended.
# Exit program if function failed
#
function exitIfFails(){
    if [ $? -ne 0 ]; then
        echo "[ERR]: unable to complete $1"
        exit -1
    else
        echo "[OK] Successfully completed $1"
    fi
}


# ------------------------------------------------------------------------------
# EXECUTION
# ------------------------------------------------------------------------------

echo -e '\n***** Start compiling project *****\n'

# ocamllex
echo -e "\n --- Generation $LEXER_FILE file..."
ocamllex -o $LEXER_GENERATED $LEXER_FILE
exitIfFails 'ocamllex'


# compile lexer.ml
echo -e "\n --- Compiling $LEXER_GENERATED file..."
ocamlopt -o $LEXER_COMPILED $LEXER_GENERATED
exitIfFails 'generated lexer compilation'

