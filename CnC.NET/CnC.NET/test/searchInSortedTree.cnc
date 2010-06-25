//********************************************************************************
// Copyright (c) 2007-2008 Intel Corporation. All rights reserved.              **
//                                                                              **
// Redistribution and use in source and binary forms, with or without           **
// modification, are permitted provided that the following conditions are met:  **
//   * Redistributions of source code must retain the above copyright notice,   **
//     this list of conditions and the following disclaimer.                    **
//   * Redistributions in binary form must reproduce the above copyright        **
//     notice, this list of conditions and the following disclaimer in the      **
//     documentation and/or other materials provided with the distribution.     **
//   * Neither the name of Intel Corporation nor the names of its contributors  **
//     may be used to endorse or promote products derived from this software    **
//     without specific prior written permission.                               **
//                                                                              **
// This software is provided by the copyright holders and contributors "as is"  **
// and any express or implied warranties, including, but not limited to, the    **
// implied warranties of merchantability and fitness for a particular purpose   **
// are disclaimed. In no event shall the copyright owner or contributors be     **
// liable for any direct, indirect, incidental, special, exemplary, or          **
// consequential damages (including, but not limited to, procurement of         **
// substitute goods or services; loss of use, data, or profits; or business     **
// interruption) however caused and on any theory of liability, whether in      **
// contract, strict liability, or tort (including negligence or otherwise)      **
// arising in any way out of the use of this software, even if advised of       **
// the possibility of such damage.                                              **
//********************************************************************************


// declarations
<searchTag: int node, int i>;
[int searchVal: int i];
[myNodeStruct treeNode: int node];
<success: int node, int i>;	// identifier of the node where the value was found. 
				// if the value was not found, no tag will be produced.

<searchTag> :: (search);

// program inputs and outputs
env -> <searchTag: root, i>;	// the root of the tree for each input
env -> [searchVal];		// one for each value we are to search for
env -> [treeNode]; 		// the whole tree
<success> -> env;

// producer/consumer relations
[searchVal], [treeNode] -> (search);
(search) -> <searchTag>, 	// continue search either right or left child
            <success>;		// value found.

