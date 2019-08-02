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
-module(dubbo_java_type_defined).
-include("java_type.hrl").
%% API
-export([get_list/0]).


get_list() ->
    [
        {java_stack_trace_element, <<"java.lang.StackTraceElement">>, record_info(fields, java_stack_trace_element)},
        {java_null_pointer_exception, <<"java.lang.NullPointerException">>, record_info(fields, java_null_pointer_exception)},
        {java_runtime_exception, <<"java.lang.RuntimeException">>, record_info(fields, java_runtime_exception)},
        {index_out_bounds_exception, <<"java.lang.IndexOutOfBoundsException">>, record_info(fields, index_out_bounds_exception)},
        {java_string_index_out_bounds_exception, <<"java.lang.StringIndexOutOfBoundsException">>, record_info(fields, java_string_index_out_bounds_exception)},
        {java_array_index_out_bounds_exception, <<"java.lang.ArrayIndexOutOfBoundsException">>, record_info(fields, java_array_index_out_bounds_exception)},
        {java_arithmetic_exception, <<"java.lang.ArithmeticException">>, record_info(fields, java_arithmetic_exception)},
        {java_class_cast_exception, <<"java.lang.ClassCastException">>, record_info(fields, java_class_cast_exception)},
        {java_nio_provider_not_found_exception, <<"java.nio.file.ProviderNotFoundException">>, record_info(fields, java_nio_provider_not_found_exception)},
        {java_security_exception, <<"java.lang.SecurityException">>, record_info(fields, java_security_exception)},
        {java_annotation_type_mismatch_exception, <<"java.lang.AnnotationTypeMismatchException">>, record_info(fields, java_annotation_type_mismatch_exception)},
        {dubbo_rpc_exception, <<"org.apache.dubbo.rpc.RpcException">>, record_info(fields, dubbo_rpc_exception)},
        {java_enum_constant_not_present_exception, <<"java.lang.EnumConstantNotPresentException">>, record_info(fields, java_enum_constant_not_present_exception)},
        {java_no_such_element_exception, <<"java.util.NoSuchElementException">>, record_info(fields, java_no_such_element_exception)},
        {java_input_mismatch_exception, <<"java.util.InputMismatchException">>, record_info(fields, java_input_mismatch_exception)},
        {dubbo_hessian_exception, <<"com.alibaba.com.caucho.hessian.HessianException">>, record_info(fields, dubbo_hessian_exception)},
        {java_wrong_method_type_exception, <<"java.lang.invoke.WrongMethodTypeException">>, record_info(fields, java_wrong_method_type_exception)},
        {java_incomplete_annotation_exception, <<"java.lang.annotation.IncompleteAnnotationException">>, record_info(fields, java_incomplete_annotation_exception)},
        {java_malformed_parameters_exception, <<"java.lang.reflect.MalformedParametersException">>, record_info(fields, java_malformed_parameters_exception)},
        {java_undeclared_throwable_exception, <<"java.lang.reflect.UndeclaredThrowableException">>, record_info(fields, java_undeclared_throwable_exception)},
        {dubbo_no_such_property_exception, <<"org.apache.dubbo.common.bytecode.NoSuchPropertyException">>, record_info(fields, dubbo_no_such_property_exception)},
        {java_mirrored_types_exception, <<"javax.lang.model.type.MirroredTypesException">>, record_info(fields, java_mirrored_types_exception)},
        {dubbo_no_such_method_exception, <<"org.apache.dubbo.common.bytecode.NoSuchMethodException">>, record_info(fields, dubbo_no_such_method_exception)},
        {java_unchecked_io_exception, <<"java.io.UncheckedIOException">>, record_info(fields, java_unchecked_io_exception)},
        {java_illegal_monitor_state_exception, <<"java.lang.IllegalMonitorStateException">>, record_info(fields, java_illegal_monitor_state_exception)},
        {java_negative_array_size_exception, <<"java.lang.NegativeArraySizeException">>, record_info(fields, java_negative_array_size_exception)},
        {java_unsupported_operation_exception, <<"java.lang.UnsupportedOperationException">>, record_info(fields, java_unsupported_operation_exception)},
        {java_empty_stack_exception, <<"java.util.EmptyStackException">>, record_info(fields, java_empty_stack_exception)},
        {java_illegal_state_exception, <<"java.lang.IllegalStateException">>, record_info(fields, java_illegal_state_exception)},
        {java_datetime_exception, <<"java.time.DateTimeException">>, record_info(fields, java_datetime_exception)},
        {java_completion_exception, <<"java.util.concurrent.CompletionException">>, record_info(fields, java_completion_exception)},
        {java_malformed_parameterized_type_exception, <<"java.lang.reflect.MalformedParameterizedTypeException">>, record_info(fields, java_malformed_parameterized_type_exception)},
        {dubbo_service_generic_exception, <<"org.apache.dubbo.rpc.service.GenericException">>, record_info(fields, dubbo_service_generic_exception)},
        {java_illegal_argument_exception, <<"java.lang.IllegalArgumentException">>, record_info(fields, java_illegal_argument_exception)},
        {java_missing_resource_pointer_exception, <<"java.util.MissingResourceException">>, record_info(fields, java_missing_resource_pointer_exception)}
    ].