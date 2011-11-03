/*
 *  R : A Computer Language for Statistical Data Analysis
 *  Copyright (C) 2011   The R Development Core Team.
 *
 *  This program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2 of the License, or
 *  (at your option) any later version.
 *
 *  This program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with this program; if not, a copy is available at
 *  http://www.r-project.org/Licenses/
 */


/* Mingw.org headers are missing this */

// temporarily copied to the raster package from parrallel
// while figuring out how to move from snow
// to parallel, and using the detectCores function from parallel 
// also in R versions prior to 2.14.0


typedef enum _LOGICAL_PROCESSOR_RELATIONSHIP {
    RelationProcessorCore,
    RelationNumaNode,
    RelationCache,
    RelationProcessorPackage,
    RelationGroup,
    RelationAll=0xffff
} LOGICAL_PROCESSOR_RELATIONSHIP;

typedef enum _PROCESSOR_CACHE_TYPE {
    CacheUnified,CacheInstruction,CacheData,CacheTrace
} PROCESSOR_CACHE_TYPE;

typedef struct _CACHE_DESCRIPTOR {
    BYTE Level;
    BYTE Associativity;
    WORD LineSize;
    DWORD Size;
    PROCESSOR_CACHE_TYPE Type;
} CACHE_DESCRIPTOR,*PCACHE_DESCRIPTOR;

typedef struct _SYSTEM_LOGICAL_PROCESSOR_INFORMATION {
    ULONG_PTR ProcessorMask;
    LOGICAL_PROCESSOR_RELATIONSHIP Relationship;
    union {
	struct {
	    BYTE Flags;
	} ProcessorCore;
	struct {
	    DWORD NodeNumber;
	} NumaNode;
	CACHE_DESCRIPTOR Cache;
	ULONGLONG Reserved[2];
    };
} SYSTEM_LOGICAL_PROCESSOR_INFORMATION,*PSYSTEM_LOGICAL_PROCESSOR_INFORMATION;

WINBASEAPI WINBOOL WINAPI 
GetLogicalProcessorInformation(PSYSTEM_LOGICAL_PROCESSOR_INFORMATION Buffer,
			       PDWORD ReturnedLength);