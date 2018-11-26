#include <Rcpp.h>
using namespace Rcpp;

// Enable C++11 via this plugin (Rcpp 0.10.3 or later)
// [[Rcpp::plugins(cpp11)]]

int getEncPosition(Rcpp::String str_to_find, Rcpp::CharacterVector vector_to_search){
  int index = -1;
  bool found=false;
  // for(int i =0; i < vector_to_search.size(); i++){ // Loop through input
  //   if(vector_to_search(i)==str_to_find){ // break out of loop
  //     found=true;
  //     index=i;
  //     break;
  //   }
  // }
  for(CharacterVector::iterator it = vector_to_search.begin(); it != vector_to_search.end(); ++it) {
    index+=1;
    if(*it==str_to_find){ // break out of loop
      found=true;
      
      break;
    }
  }
  
  if(found==false){
    return -1;
  }else {
    return index;
  }

};

// [[Rcpp::export]]
NumericVector flow_Char_Vector(std::string stringEntry, Rcpp::CharacterVector encoding) {
  NumericMatrix outputArray(stringEntry.length(),encoding.size());
  int position;
  int stringLength = (int)stringEntry.length();
  for(int i =0; i < stringLength; i++){
    position = getEncPosition(stringEntry.substr( i, 1 ),encoding);
    if (position == -1) {
      continue;
    }else{
      outputArray(i,position)=1;
    }
  }
  return outputArray;
}

// NumericMatrix flow_Char_To_Tensor(NumericVector inputarray, Rcpp::NumericVector nchar){
// 
// 
// 
// }
