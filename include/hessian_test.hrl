% ---------------------------------------------------------------------------
%   Copyright (C) 2008 0x6e6562
%
%   Licensed under the Apache License, Version 2.0 (the "License");
%   you may not use this file except in compliance with the License.
%   You may obtain a copy of the License at
%
%       http://www.apache.org/licenses/LICENSE-2.0
%
%   Unless required by applicable law or agreed to in writing, software
%   distributed under the License is distributed on an "AS IS" BASIS,
%   WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%   See the License for the specific language governing permissions and
%   limitations under the License.
% ---------------------------------------------------------------------------

% This is for simple tests
-record(pair, {first, second}).

% These are used in the kitchen sink tests
-record(kitchen_sink,  {color, 
                        plug_hole,
                        delivery_date,
                        cost,
                        type,
                        deluxe}).
-record(plug_hole, {diameter}).
-record(specification, {customer_name, 
                        plug_hole,
                        color,
                        total,
                        type,
                        order_date,
                        deluxe}).