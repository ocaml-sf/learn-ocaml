// $Id: readconic.cpp 1898 2013-04-09 18:06:04Z stefan $
// Copyright (C) 2005, International Business Machines
// Corporation and others.  All Rights Reserved.
// This code is licensed under the terms of the Eclipse Public License (EPL).

#include "CoinMpsIO.hpp"

int main (int argc, const char *argv[])
{
  CoinMpsIO m_MpsData;
  int nOfSOS;
  CoinSet ** SOS = NULL;
  std::string mpsFileName;
#if defined(SAMPLEDIR)
  mpsFileName = SAMPLEDIR "/conic.mps";
#else
  if (argc < 2) {
    fprintf(stderr, "Do not know where to find sample MPS files.\n");
    exit(1);
  }
#endif
  if (argc>=2) mpsFileName = argv[1];
  int status = m_MpsData.readMps( mpsFileName.c_str(), "", nOfSOS, SOS );
  assert (!status);
  int * columnStart = NULL;
  int * columnIdx = NULL;
  double * elements = NULL;
  status = m_MpsData.readQuadraticMps(NULL, columnStart, columnIdx, elements, 0);
  assert (!status);
  int nOfCones;
  int * coneStart = NULL;
  int * coneIdx = NULL;
  int * coneType = NULL;
  status = m_MpsData.readConicMps(NULL, coneStart, coneIdx, coneType, nOfCones);
  assert (!status);
  if (nOfSOS) {
    printf("%d SOS sets\n",nOfSOS);
    for (int iSOS=0;iSOS<nOfSOS;iSOS++) {
      int numberEntries = SOS[iSOS]->numberEntries();
      printf("Set %d has %d entries - type %d\n",iSOS,numberEntries,SOS[iSOS]->setType());
      const int * which = SOS[iSOS]->which();
      const double * weights = SOS[iSOS]->weights();
      for (int i=0;i<numberEntries;i++)
	printf("(%d wt %g) ",which[i],weights[i]);
      printf("\n");
      delete SOS[iSOS];
    }
  }
  delete [] SOS;
  int numberColumns=m_MpsData.getNumCols();
  if (columnStart[numberColumns]) {
    printf("Quadratic objective has %d entries\n",columnStart[numberColumns]);
    for (int iColumn=0;iColumn<numberColumns;iColumn++) {
      if(columnStart[iColumn]<columnStart[iColumn+1]) {
	printf("Column %d obj ",iColumn);
	for (int j=columnStart[iColumn];j<columnStart[iColumn+1];j++)
	  printf("(%d,%g) ",columnIdx[j],elements[j]);
	printf("\n");
      }
    }
  }
  delete [] columnStart;
  delete [] columnIdx;
  delete [] elements;
  if (nOfCones) {
    printf("Conic section has %d cones\n",nOfCones);
    for (int iCone=0;iCone<nOfCones;iCone++) {
      printf("Cone %d has %d entries (type %d) ",iCone,coneStart[iCone+1]-coneStart[iCone],
	     coneType[iCone]);
      for (int j=coneStart[iCone];j<coneStart[iCone+1];j++)
	printf("%d ",coneIdx[j]);
      printf("\n");
    }
  }
  delete [] coneStart;
  delete [] coneIdx;
  delete [] coneType;
  return 0;
}    
