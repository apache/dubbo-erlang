%%------------------------------------------------------------------------------
%% Licensed to the Apache Software Foundation (ASF) under one or more
%% contributor license agreements.  See the NOTICE file distributed with
%% this work for additional information regarding copyright ownership.
%% The ASF licenses this file to You under the Apache License, Version 2.0
%% (the "License"); you may not use this file except in compliance with
%% the License.  You may obtain a copy of the License at
%%
%%     http://www.apache.org/licenses/LICENSE-2.0
%%
%% Unless required by applicable law or agreed to in writing, software
%% distributed under the License is distributed on an "AS IS" BASIS,
%% WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
%% See the License for the specific language governing permissions and
%% limitations under the License.
%%------------------------------------------------------------------------------
-record(java_stack_trace_element, {declaringClass, methodName, fileName, lineNumber}).
-record(java_null_pointer_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_runtime_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(index_out_bounds_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_string_index_out_bounds_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_array_index_out_bounds_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_arithmetic_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_class_cast_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_nio_provider_not_found_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_security_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_annotation_type_mismatch_exception, {detailMessage, cause, stackTrace, suppressedExceptions, element, foundType}).
-record(dubbo_rpc_exception, {detailMessage, cause, stackTrace, suppressedExceptions, code}).
-record(java_enum_constant_not_present_exception, {detailMessage, cause, stackTrace, suppressedExceptions, enumType, constantName}).
-record(java_no_such_element_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_input_mismatch_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(dubbo_hessian_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_wrong_method_type_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_incomplete_annotation_exception, {detailMessage, cause, stackTrace, suppressedExceptions, annotationType, elementName}).
-record(java_malformed_parameters_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_undeclared_throwable_exception, {detailMessage, cause, stackTrace, suppressedExceptions, undeclaredThrowable}).
-record(dubbo_no_such_property_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_mirrored_types_exception, {detailMessage, cause, stackTrace, suppressedExceptions, types}).
-record(dubbo_no_such_method_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_unchecked_io_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_illegal_monitor_state_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_negative_array_size_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_unsupported_operation_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_empty_stack_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_illegal_state_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_datetime_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_completion_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_malformed_parameterized_type_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(dubbo_service_generic_exception, {detailMessage, cause, stackTrace, suppressedExceptions, exceptionClass, exceptionMessage}).
-record(java_illegal_argument_exception, {detailMessage, cause, stackTrace, suppressedExceptions}).
-record(java_missing_resource_pointer_exception, {detailMessage, cause, stackTrace, suppressedExceptions, className, key}).