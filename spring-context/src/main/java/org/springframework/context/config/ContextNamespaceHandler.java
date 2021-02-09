/*
 * Copyright 2002-2012 the original author or authors.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *      https://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package org.springframework.context.config;

import org.springframework.beans.factory.xml.NamespaceHandlerSupport;
import org.springframework.context.annotation.AnnotationConfigBeanDefinitionParser;
import org.springframework.context.annotation.ComponentScanBeanDefinitionParser;

/**
 * {@link org.springframework.beans.factory.xml.NamespaceHandler}
 * for the '{@code context}' namespace.
 * <p>
 * context 标签解析的类的初始化类
 *
 * @author Mark Fisher
 * @author Juergen Hoeller
 * @since 2.5
 */
public class ContextNamespaceHandler extends NamespaceHandlerSupport {

	@Override
	public void init() {
		//注册property-placeholder 解析器
		//<context:property-placeholder location="classpath*:jdbc.properties"/>
		registerBeanDefinitionParser("property-placeholder", new PropertyPlaceholderBeanDefinitionParser());

		//注册property-override注册器
		registerBeanDefinitionParser("property-override", new PropertyOverrideBeanDefinitionParser());

		//注册annotation-config解析器
		//<contex:annotation-config/>
		registerBeanDefinitionParser("annotation-config", new AnnotationConfigBeanDefinitionParser());

		//注册component-scan解析器
		//<context:component-scan base-package="com.xiangxue.jack"/>
		registerBeanDefinitionParser("component-scan", new ComponentScanBeanDefinitionParser());

		registerBeanDefinitionParser("load-time-weaver", new LoadTimeWeaverBeanDefinitionParser());
		registerBeanDefinitionParser("spring-configured", new SpringConfiguredBeanDefinitionParser());
		registerBeanDefinitionParser("mbean-export", new MBeanExportBeanDefinitionParser());
		registerBeanDefinitionParser("mbean-server", new MBeanServerBeanDefinitionParser());
	}

}
