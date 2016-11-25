/////////////////////////////////////////////////////
// DALY.cpp
// C++ interaction file for R DALY Package
//
// <author>     brechtdv@gmail.com
// <version>    1.5.0
// <date>       2016/11/25
//

#include <iostream>
#include <vector>
using namespace std;

#include "R.h"
#include "Rmath.h"

vector<double> getSamples(double data[], int dist[], int strat[], int par, int outcomes, int OClist[], int iterations)
{
	int nGroups[] = {10,5,2,1};
	int XX[]  = {0,1,2,3,4,10,11,12,13,14,0,1,2,3,4,10,11,12,13,14,0,10,2,3,4,10,11,12,13,14,0,1,2,3,4,10,11,12,13,14};
	int XXX[] = {0,1,2,3,4,15,16,17,18,19,0,1,2,3,4,15,16,17,18,19,0,15,2,3,4,15,16,17,18,19,0,1,2,3,4,15,16,17,18,19};
	vector<double> samples;

	GetRNGstate();

	for (int o=0; o<outcomes; o++)
	{
		int thisOutcome = 30*(OClist[o]-1);
		double thisData[30];
		for (int i=0; i<30; i++) thisData[i] = data[i+thisOutcome];
		int thisPoint = par + 8*(OClist[o]-1);
		int thisDist = dist[thisPoint];
		int thisStrat = strat[thisPoint]-1;

		switch(thisDist)
		{
			case 1:     // Beta-pert
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					int point1 = XXX[n+(thisStrat)*10];
					int point2 = XXX[n+(thisStrat)*10]+5;
					int point3 = XXX[n+(thisStrat)*10]+10;
					double mode  = thisData[point1];
					double min   = thisData[point2];
					double max   = thisData[point3];

					double mean  = (max + 4*mode + min) /   6;
					double sdev  = (max - min) / 6;
					double alpha = ((mean-min)/(max-min)) * (((mean-min)*(max-mean))/(pow(sdev,2)) - 1);
					double beta  = ((max-mean)/(mean-min)) * alpha;
					if (alpha>0 && beta>0)
					{
						for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(rbeta(alpha,beta)*(max-min)+min);
					} else {
						for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(0);
					}
				}
			} break;

			case 2:     // Beta
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					int point1 = XX[n+(thisStrat)*10];
					int point2 = XX[n+(thisStrat)*10]+5;
					double alpha = thisData[point1];
					double beta = thisData[point2];
					if (alpha>0 && beta>0)
					{
						for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(rbeta(alpha,beta));
					} else {
						for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(0);
					}
				}
			} break;

			case 3:     // Gamma
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					int point1 = XX[n+(thisStrat)*10];
					int point2 = XX[n+(thisStrat)*10]+5;
					double shape = thisData[point1];
					double rate = thisData[point2];
					if (shape>0 && rate>0)
					{
						for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(rgamma(shape,1/rate));
					} else {
						for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(0);
					}
				}
			} break;

			case 4:     // Normal
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					int point1 = XX[n+(thisStrat)*10];
					int point2 = XX[n+(thisStrat)*10]+5;
					double mean = thisData[point1];
					double sdev = thisData[point2];
					for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(rnorm(mean,sdev));
				}
			} break;

			case 5:     // LogNormal - geometric
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					int point1 = XX[n+(thisStrat)*10];
					int point2 = XX[n+(thisStrat)*10]+5;
					double mean = thisData[point1];
					double sdev = thisData[point2];
					for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(rlnorm(mean,sdev));
				}
			} break;

			case 6:     // LogNormal - arithmetic
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					int point1 = XX[n+(thisStrat)*10];
					int point2 = XX[n+(thisStrat)*10]+5;
					double lmean = thisData[point1];
					double lvar = pow(thisData[point2],2);
					double mean = log(lmean) - 0.5*log(1+(lvar/pow(lmean,2)));
					double sdev = sqrt(log(1+(lvar/pow(lmean,2))));
					for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(rlnorm(mean,sdev));
				}
			} break;

			case 7:     // Uniform
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					int point1 = XX[n+(thisStrat)*10];
					int point2 = XX[n+(thisStrat)*10]+5;
					double min = thisData[point1];
					double max = thisData[point2];
					for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(runif(min,max));
				}
			} break;

			case 8:     // Fixed
			{
				for (int n=0; n<nGroups[thisStrat]; n++)
				{
					for (int x=0; x<(iterations*(10/nGroups[thisStrat])); x++) samples.push_back(thisData[n]);
				}
			} break;

		}
	}

	PutRNGstate();

	return(samples);
}

extern "C" {

    double getLxp(double age, int sex, double lxpList[])
    {
        int point = fround(age*10, 0) + sex*951;
        double value = lxpList[point];
        return(value);
    }

    double formula(double rate, int K, double start, double stop)
    {
        double C = 0.1658;
        double B = 0.04;
        double result;
        result = (rate>0) ? (K * ((C * exp(rate * start)) / pow((rate+B),2)) * ((exp(-(rate+B) * (start+stop)) * (-(rate+B) * (start+stop) -1 )) - (exp(-(rate+B) * start) * (-(rate+B) * start - 1))) + ((1-K)/rate) * ((1-exp(-rate*stop)))) : K * C * ((exp(-B*start))/pow(B,2))*((exp(-B*stop))*(-B*(start+stop)-1)-(-B*start-1))+((1-K)*stop);
        return(result);
    }

    void getMC(double *MRT, double *INC, double *YLD, double *YLL,
               double *samplesInc, double *samplesTrt, double *samplesOns, double *samplesDur,
               double *samplesDWt, double *samplesDWn, double *samplesMrt, double *samplesDth,
               int *IT, int *AW, double *DR,
               int *OC, int *nOC, int *getDist, int *getStrat, int *getStrAge, int *getStrSex,
               double *getPop, double *getDur, double *getOns, double *getInc, double *getTrt,
               double *getMrt, double *getDWt, double *getDWn, double *getDth, double *listLxp)
    {
        double D = (double)*DR/100;
        int ageGroups = 5;

        vector<double> SamplesInc = getSamples(getInc, getDist, getStrat, 0, *nOC, OC, *IT);
        vector<double> SamplesTrt = getSamples(getTrt, getDist, getStrat, 1, *nOC, OC, *IT);
        vector<double> SamplesOns = getSamples(getOns, getDist, getStrat, 2, *nOC, OC, *IT);
        vector<double> SamplesDur = getSamples(getDur, getDist, getStrat, 3, *nOC, OC, *IT);
        vector<double> SamplesDWt = getSamples(getDWt, getDist, getStrat, 4, *nOC, OC, *IT);
        vector<double> SamplesDWn = getSamples(getDWn, getDist, getStrat, 5, *nOC, OC, *IT);
        vector<double> SamplesMrt = getSamples(getMrt, getDist, getStrat, 6, *nOC, OC, *IT);
        vector<double> SamplesDth = getSamples(getDth, getDist, getStrat, 7, *nOC, OC, *IT);

        for (int o=0; o<*nOC; o++) // iterate over outcomes
        {
			unsigned int thisOC = o * 10 * (*IT);
			unsigned int thisStr = 8*(OC[o]-1);
			
            for (int s=0; s<2; s++) // iterate over sexes
            {
				unsigned int thisS = s * 5 * (*IT);
				
				for (int a=0; a<ageGroups; a++) // iterate over age groups
				{
					unsigned int thisAG = a * (*IT);
									
					for (int i=0; i<*IT; i++) // generate 'IT' simulations
                    {
                        double inc = getPop[a+5*s] * (SamplesInc[ (thisOC) + a*getStrAge[thisStr]*(*IT) + s*getStrSex[thisStr]*(*IT) + i ] )/1000;
                        double trt = SamplesTrt[ (thisOC) + a*getStrAge[thisStr+1]*(*IT) + s*getStrSex[thisStr+1]*(*IT) + i ];
                        double ons = SamplesOns[ (thisOC) + a*getStrAge[thisStr+2]*(*IT) + s*getStrSex[thisStr+2]*(*IT) + i ];
                        double dur = SamplesDur[ (thisOC) + a*getStrAge[thisStr+3]*(*IT) + s*getStrSex[thisStr+3]*(*IT) + i ];
                        double DWt = SamplesDWt[ (thisOC) + a*getStrAge[thisStr+4]*(*IT) + s*getStrSex[thisStr+4]*(*IT) + i ];
                        double DWn = SamplesDWn[ (thisOC) + a*getStrAge[thisStr+5]*(*IT) + s*getStrSex[thisStr+5]*(*IT) + i ];
                        double mrt = getPop[a+5*s] * (SamplesMrt[ (thisOC) + a*getStrAge[thisStr+6]*(*IT) + s*getStrSex[thisStr+6]*(*IT) + i ] )/1000;
                        double dth = SamplesDth[ (thisOC) + a*getStrAge[thisStr+7]*(*IT) + s*getStrSex[thisStr+7]*(*IT) + i ];
                        double lxp = getLxp(dth, s, listLxp);

                        double YLDi = inc * formula(D,*AW,ons,dur) * ((trt * DWt) + ((1-trt) * DWn));
                        double YLLi = mrt * formula(D,*AW,dth,lxp);

                        INC[thisOC + thisS + thisAG + i] += inc;
                        MRT[thisOC + thisS + thisAG + i] += mrt;
                        YLD[thisOC + thisS + thisAG + i] += YLDi;
                        YLL[thisOC + thisS + thisAG + i] += YLLi;
						
                        samplesInc[thisOC + thisS + thisAG + i] = SamplesInc[ (thisOC) + a*getStrAge[thisStr]*(*IT) + s*getStrSex[thisStr]*(*IT) + i ];
                        samplesMrt[thisOC + thisS + thisAG + i] = SamplesMrt[ (thisOC) + a*getStrAge[thisStr+6]*(*IT) + s*getStrSex[thisStr+6]*(*IT) + i ];
                        samplesTrt[thisOC + thisS + thisAG + i] = trt;
                        samplesOns[thisOC + thisS + thisAG + i] = ons;
                        samplesDur[thisOC + thisS + thisAG + i] = dur;
                        samplesDWt[thisOC + thisS + thisAG + i] = DWt;
                        samplesDWn[thisOC + thisS + thisAG + i] = DWn;
                        samplesDth[thisOC + thisS + thisAG + i] = dth;
                    }
                }
            }
        }

        return;

    }
}
