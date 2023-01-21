#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
#include <map>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0
#define CLAUSE_SIZE 3

struct Variable {
    vector<int> PosLit;
    vector<int> NegLit;
    long long heuristic; 
};

uint numVars;
uint numClauses;
vector<vector<int>> clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;

vector<Variable> Variables;

const int INCREMENT = 10;

void readClauses(){
  // Skip comments
    char c = cin.get();
    while (c == 'c') {
        while (c != '\n') c = cin.get();
        c = cin.get();
    }  
    // Read "cnf numVars numClauses"
    string aux;
    cin >> aux >> numVars >> numClauses;
    clauses.resize(numClauses);  
    Variables.resize(numVars+1);
    // Read clauses
    for (uint i = 0; i < numClauses; ++i) {
        int lit;
        while (cin >> lit and lit != 0) {
            clauses[i].push_back(lit);
            if (lit > 0) Variables[lit].PosLit.push_back(i);
            else Variables[-lit].NegLit.push_back(i);
        }   
    }
    //El heurístico es más alto para los literales que más aparecen.
    for (uint i = 1; i <= numVars; ++i) {
        Variables[i].heuristic = Variables[i].PosLit.size()*10 + Variables[i].NegLit.size()*10;
    }
}

int currentValueInModel(int lit){
    if (lit >= 0) return model[lit];
    else {
        if (model[-lit] == UNDEF) return UNDEF;
        else return 1 - model[-lit];
    }
}

void setLiteralToTrue(int lit){
    modelStack.push_back(lit);
    if (lit > 0) model[lit] = TRUE;
    else model[-lit] = FALSE;		
}

// Heuristic for finding the next decision literal:
// Esta función hace que el literal con más actividad sea el siguiente literal de decisión.
int getNextDecisionLiteral(){ 
    int lit = 0;
    long max = -1;
    //Escogemos el primer literal indefinido.
    for (uint i = 1; i <= numVars; ++i) {
        if (model[i] == UNDEF and Variables[i].heuristic > max){
            max = Variables[i].heuristic;
            lit = i;
        }
    }
    //Se escoge el literal (positivo o negativo) que aparezca más veces.
    if (Variables[lit].PosLit.size() > Variables[lit].NegLit.size()) return lit;
    return -lit;
}

void updateHeuristic(int lit) {
    Variables[abs(lit)].heuristic += INCREMENT;
}


bool propagarLiteral(const vector<int>& clauseList) {
    for (uint i = 0; i < clauseList.size(); ++i) {
        bool someLitTrue = false;
        int numUndefs = 0;
        int lastLitUndef = 0;
        int index = clauseList[i];
        
        for (int k = 0; not someLitTrue and k < CLAUSE_SIZE; ++k) {
            int val = currentValueInModel(clauses[index][k]);
            if (val == TRUE) someLitTrue = true;
            else if (val == UNDEF) {
                ++numUndefs;
                lastLitUndef = clauses[index][k];
            }
        }
        if (not someLitTrue and numUndefs == 0) {
            for (int j = 0; j < CLAUSE_SIZE; ++j) updateHeuristic(clauses[index][j]);
            return true; //Hay conflicto.
        }
        else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);
    }
    return false;
}


bool propagateGivesConflict () { //solo mirar las clausulas que tengan la variable que se propaga.
    while ( indexOfNextLitToPropagate < modelStack.size() ) {
        int lit = modelStack[indexOfNextLitToPropagate]; //el literal a propagar.
        ++indexOfNextLitToPropagate;
        //Si el literal es negativo, tenemos que mirar las cláusulas en las que aparece positivo,
        //porque esas cláusulas no sabemos si van a ser true. Las cláusulas en las que aparece el literal
        //negativo ya sabemos que se van a satisfacer.
        //Caso equivalente si el literal es positivo.
        if (lit < 0 and propagarLiteral(Variables[-lit].PosLit)) return true;
        if (lit > 0 and propagarLiteral(Variables[lit].NegLit)) return true;
    }
    return false;
}
    
   
void backtrack(){
    uint i = modelStack.size() -1;
    int lit = 0;
    while (modelStack[i] != 0){ // 0 is the DL mark
        lit = modelStack[i];
        model[abs(lit)] = UNDEF;
        modelStack.pop_back();
        --i;
    }
    // at this point, lit is the last decision
    modelStack.pop_back(); // remove the DL mark
    --decisionLevel;
    indexOfNextLitToPropagate = modelStack.size();
    setLiteralToTrue(-lit);  // reverse last decision
}

void checkmodel(){
    for (uint i = 0; i < numClauses; ++i){
        bool someTrue = false;
        for (uint j = 0; not someTrue and j < clauses[i].size(); ++j) {
            someTrue = (currentValueInModel(clauses[i][j]) == TRUE);
        }
        if (not someTrue) {
            cout << "Error in model, clause is not satisfied:";
            for (uint j = 0; j < clauses[i].size(); ++j) cout << clauses[i][j] << " ";
            cout << endl;
            exit(1);
        }
    }  
}


int main(){ 
    readClauses(); // reads numVars, numClauses and clauses
    model.resize(numVars+1,UNDEF);
    indexOfNextLitToPropagate = 0;  
    decisionLevel = 0;
    
    // Take care of initial unit clauses, if any
    for (uint i = 0; i < numClauses; ++i) {
        if (clauses[i].size() == 1) {
            int lit = clauses[i][0];
            int val = currentValueInModel(lit);
            if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
            else if (val == UNDEF) setLiteralToTrue(lit);
        }
    }
    
    // DPLL algorithm
    int counter = 0;
    while (true) {
        if (counter == 10000) {
            for (uint i = 1; i <= numVars; ++i) {
                Variables[i].heuristic /= 2;
            }
            counter = 0;
        }
        ++counter;
        while ( propagateGivesConflict() ) {
            if (decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; return 10; }
            backtrack();
        }
        int decisionLit = getNextDecisionLiteral();
        if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; return 20; }
        // start new decision level:
        modelStack.push_back(0);  // push mark indicating new DL
        ++indexOfNextLitToPropagate;
        ++decisionLevel;
        setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
    }
}  
