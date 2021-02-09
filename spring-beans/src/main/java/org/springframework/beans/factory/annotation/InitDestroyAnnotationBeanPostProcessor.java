/*
 * Copyright 2002-2019 the original author or authors.
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

package org.springframework.beans.factory.annotation;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.Collection;
import java.util.Collections;
import java.util.LinkedHashSet;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.config.DestructionAwareBeanPostProcessor;
import org.springframework.beans.factory.support.MergedBeanDefinitionPostProcessor;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.core.Ordered;
import org.springframework.core.PriorityOrdered;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.lang.Nullable;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;

/**
 * {@link org.springframework.beans.factory.config.BeanPostProcessor} implementation
 * that invokes annotated init and destroy methods. Allows for an annotation
 * alternative to Spring's {@link org.springframework.beans.factory.InitializingBean}
 * and {@link org.springframework.beans.factory.DisposableBean} callback interfaces.
 *
 * <p>The actual annotation types that this post-processor checks for can be
 * configured through the {@link #setInitAnnotationType "initAnnotationType"}
 * and {@link #setDestroyAnnotationType "destroyAnnotationType"} properties.
 * Any custom annotation can be used, since there are no required annotation
 * attributes.
 *
 * <p>Init and destroy annotations may be applied to methods of any visibility:
 * public, package-protected, protected, or private. Multiple such methods
 * may be annotated, but it is recommended to only annotate one single
 * init method and destroy method, respectively.
 *
 * <p>Spring's {@link org.springframework.context.annotation.CommonAnnotationBeanPostProcessor}
 * supports the JSR-250 {@link javax.annotation.PostConstruct} and {@link javax.annotation.PreDestroy}
 * annotations out of the box, as init annotation and destroy annotation, respectively.
 * Furthermore, it also supports the {@link javax.annotation.Resource} annotation
 * for annotation-driven injection of named beans.
 *
 * @author Juergen Hoeller
 * @see #setInitAnnotationType
 * @see #setDestroyAnnotationType
 * @since 2.5
 */
@SuppressWarnings("serial")
public class InitDestroyAnnotationBeanPostProcessor
		implements DestructionAwareBeanPostProcessor, MergedBeanDefinitionPostProcessor, PriorityOrdered, Serializable {

	private final transient LifecycleMetadata emptyLifecycleMetadata =
			new LifecycleMetadata(Object.class, Collections.emptyList(), Collections.emptyList()) {
				@Override
				public void checkConfigMembers(RootBeanDefinition beanDefinition) {
				}

				@Override
				public void invokeInitMethods(Object target, String beanName) {
				}

				@Override
				public void invokeDestroyMethods(Object target, String beanName) {
				}

				@Override
				public boolean hasDestroyMethods() {
					return false;
				}
			};


	protected transient Log logger = LogFactory.getLog(getClass());

	@Nullable
	private Class<? extends Annotation> initAnnotationType;

	@Nullable
	private Class<? extends Annotation> destroyAnnotationType;

	private int order = Ordered.LOWEST_PRECEDENCE;

	@Nullable
	private final transient Map<Class<?>, LifecycleMetadata> lifecycleMetadataCache = new ConcurrentHashMap<>(256);


	/**
	 * Specify the init annotation to check for, indicating initialization
	 * methods to call after configuration of a bean.
	 * <p>Any custom annotation can be used, since there are no required
	 * annotation attributes. There is no default, although a typical choice
	 * is the JSR-250 {@link javax.annotation.PostConstruct} annotation.
	 */
	public void setInitAnnotationType(Class<? extends Annotation> initAnnotationType) {
		this.initAnnotationType = initAnnotationType;
	}

	/**
	 * Specify the destroy annotation to check for, indicating destruction
	 * methods to call when the context is shutting down.
	 * <p>Any custom annotation can be used, since there are no required
	 * annotation attributes. There is no default, although a typical choice
	 * is the JSR-250 {@link javax.annotation.PreDestroy} annotation.
	 */
	public void setDestroyAnnotationType(Class<? extends Annotation> destroyAnnotationType) {
		this.destroyAnnotationType = destroyAnnotationType;
	}

	public void setOrder(int order) {
		this.order = order;
	}

	@Override
	public int getOrder() {
		return this.order;
	}

	/**
	 * 扫描方法的initMethod和destroyMethod并将对应的注解封装成LifecycleMetadata
	 *
	 * @param beanDefinition the merged bean definition for the bean
	 * @param beanType       the actual type of the managed bean instance
	 * @param beanName       the name of the bean
	 */
	@Override
	public void postProcessMergedBeanDefinition(RootBeanDefinition beanDefinition, Class<?> beanType, String beanName) {
		//1. 根据class获取LifecycleMetadata封装了 initMethod和destroyMethod的列表
		LifecycleMetadata metadata = findLifecycleMetadata(beanType);
		//2. 检测BeanDefinition和LifecycleElement的对应关系并给全局的checkedInitMethods，checkedDestroyMethods赋值
		metadata.checkConfigMembers(beanDefinition);
	}

	/**
	 * 实例化后调用initMethod
	 *
	 * @param bean
	 * @param beanName
	 * @return
	 * @throws BeansException
	 */
	@Override
	public Object postProcessBeforeInitialization(Object bean, String beanName) throws BeansException {
		//1. 从上一步搜集到的LifecycleMetadata中获取metadata
		LifecycleMetadata metadata = findLifecycleMetadata(bean.getClass());
		try {
			//2. 调用InitMethod
			metadata.invokeInitMethods(bean, beanName);
		} catch (InvocationTargetException ex) {
			throw new BeanCreationException(beanName, "Invocation of init method failed", ex.getTargetException());
		} catch (Throwable ex) {
			throw new BeanCreationException(beanName, "Failed to invoke init method", ex);
		}
		return bean;
	}

	@Override
	public Object postProcessAfterInitialization(Object bean, String beanName) throws BeansException {
		return bean;
	}

	/**
	 * 调用有@Destroy注解的销毁方法
	 *
	 * @param bean
	 * @param beanName
	 * @throws BeansException
	 */
	@Override
	public void postProcessBeforeDestruction(Object bean, String beanName) throws BeansException {
		//获取对应bean的metadata
		LifecycleMetadata metadata = findLifecycleMetadata(bean.getClass());
		try {
			//调用销毁方法
			metadata.invokeDestroyMethods(bean, beanName);
		} catch (InvocationTargetException ex) {
			String msg = "Destroy method on bean with name '" + beanName + "' threw an exception";
			if (logger.isDebugEnabled()) {
				logger.warn(msg, ex.getTargetException());
			} else {
				logger.warn(msg + ": " + ex.getTargetException());
			}
		} catch (Throwable ex) {
			logger.warn("Failed to invoke destroy method on bean with name '" + beanName + "'", ex);
		}
	}

	/**
	 * 对 @Destroy注解的支持
	 * 获取bean的销毁方法
	 *
	 * @param bean
	 * @return
	 */
	@Override
	public boolean requiresDestruction(Object bean) {
		return findLifecycleMetadata(bean.getClass()).hasDestroyMethods();
	}

	/**
	 * 根据class 获取获取initMethod和destroyMethod
	 * 并封装成LifecycleMetadata
	 *
	 * @param clazz
	 * @return
	 */
	private LifecycleMetadata findLifecycleMetadata(Class<?> clazz) {
		//1. 如果缓存对象为空
		if (this.lifecycleMetadataCache == null) {
			// Happens after deserialization, during destruction...
			//2. 获取initMethod和destroyMethod并封装成LifecycleMetadata
			return buildLifecycleMetadata(clazz);
		}
		// Quick check on the concurrent map first, with minimal locking.
		//3. 从缓存中获取LifecycleMetadata
		LifecycleMetadata metadata = this.lifecycleMetadataCache.get(clazz);
		if (metadata == null) {
			synchronized (this.lifecycleMetadataCache) {
				//3.1 从缓存中获取LifecycleMetadata 防止并发情况
				metadata = this.lifecycleMetadataCache.get(clazz);
				//3.2 如果缓存中没有
				if (metadata == null) {
					//3.3 获取initMethod和destroyMethod并封装成LifecycleMetadata
					metadata = buildLifecycleMetadata(clazz);
					//3.4 加入缓存
					this.lifecycleMetadataCache.put(clazz, metadata);
				}
				return metadata;
			}
		}
		return metadata;
	}

	/**
	 * 获取initMethod和destroyMethod
	 * 并封装成LifecycleMetadata
	 *
	 * @param clazz
	 * @return
	 */
	private LifecycleMetadata buildLifecycleMetadata(final Class<?> clazz) {
		//1. 如果注解没有initMethod,destroyMethod注解则返回空的list
		if (!AnnotationUtils.isCandidateClass(clazz, Arrays.asList(this.initAnnotationType, this.destroyAnnotationType))) {
			return this.emptyLifecycleMetadata;
		}
		//initMethod列表
		List<LifecycleElement> initMethods = new ArrayList<>();
		//destroyMethod 列表
		List<LifecycleElement> destroyMethods = new ArrayList<>();
		Class<?> targetClass = clazz;

		do {
			final List<LifecycleElement> currInitMethods = new ArrayList<>();
			final List<LifecycleElement> currDestroyMethods = new ArrayList<>();
			//2. 对所有class的method进行回调
			ReflectionUtils.doWithLocalMethods(targetClass, method -> {
				//2.1  如果initAnnotationType不为空并且method中有initAnnotationType这个注解类型就创建LifecycleElement并加入到currInitMethods
				if (this.initAnnotationType != null && method.isAnnotationPresent(this.initAnnotationType)) {
					//2.2 创建LifecycleElement
					LifecycleElement element = new LifecycleElement(method);
					//2.3 加入到currInitMethods
					currInitMethods.add(element);
					if (logger.isTraceEnabled()) {
						logger.trace("Found init method on class [" + clazz.getName() + "]: " + method);
					}
				}
				//2.4 如果destroyAnnotationType不为空并且method中有destroyAnnotationType这个注解类型就创建LifecycleElement并加入到currDestroyMethods
				if (this.destroyAnnotationType != null && method.isAnnotationPresent(this.destroyAnnotationType)) {
					//2.5 创建LifecycleElement并加入到currDestroyMethods
					currDestroyMethods.add(new LifecycleElement(method));
					if (logger.isTraceEnabled()) {
						logger.trace("Found destroy method on class [" + clazz.getName() + "]: " + method);
					}
				}
			});
			//3. currInitMethods添加到initMethods
			initMethods.addAll(0, currInitMethods);
			//4. currDestroyMethods添加到destroyMethods
			destroyMethods.addAll(currDestroyMethods);
			//5. 将targetClass的父类赋值给targetClass
			targetClass = targetClass.getSuperclass();
		}
		//6. 如果父类不是object 继续循环
		while (targetClass != null && targetClass != Object.class);
		//7. 将initMethods和destroyMethods封装成LifecycleMetadata返回
		return (initMethods.isEmpty() && destroyMethods.isEmpty() ? this.emptyLifecycleMetadata :
				new LifecycleMetadata(clazz, initMethods, destroyMethods));
	}


	//---------------------------------------------------------------------
	// Serialization support
	//---------------------------------------------------------------------

	private void readObject(ObjectInputStream ois) throws IOException, ClassNotFoundException {
		// Rely on default serialization; just initialize state after deserialization.
		ois.defaultReadObject();

		// Initialize transient fields.
		this.logger = LogFactory.getLog(getClass());
	}


	/**
	 * Class representing information about annotated init and destroy methods.
	 */
	private class LifecycleMetadata {

		private final Class<?> targetClass;

		private final Collection<LifecycleElement> initMethods;

		private final Collection<LifecycleElement> destroyMethods;

		@Nullable
		private volatile Set<LifecycleElement> checkedInitMethods;

		@Nullable
		private volatile Set<LifecycleElement> checkedDestroyMethods;

		public LifecycleMetadata(Class<?> targetClass, Collection<LifecycleElement> initMethods,
								 Collection<LifecycleElement> destroyMethods) {

			this.targetClass = targetClass;
			this.initMethods = initMethods;
			this.destroyMethods = destroyMethods;
		}

		/**
		 * 检测BeanDefinition和LifecycleElement的对应关系并给全局的checkedInitMethods，checkedDestroyMethods赋值
		 *
		 * @param beanDefinition
		 */
		public void checkConfigMembers(RootBeanDefinition beanDefinition) {
			Set<LifecycleElement> checkedInitMethods = new LinkedHashSet<>(this.initMethods.size());
			//1. initMethod方法的检查以及设置
			// 1.1 遍历检查所有的收集的initMethod方法
			for (LifecycleElement element : this.initMethods) {
				String methodIdentifier = element.getIdentifier();
				// 1.2如果beanDefinition的externallyManagedConfigMembers属性不包含该methodIdentifier
				if (!beanDefinition.isExternallyManagedInitMethod(methodIdentifier)) {
					// 1.3将该methodIdentifier添加到beanDefinition的externallyManagedConfigMembers属性
					beanDefinition.registerExternallyManagedInitMethod(methodIdentifier);
					// 1.4并将element添加到checkedElements
					checkedInitMethods.add(element);
					if (logger.isTraceEnabled()) {
						logger.trace("Registered init method on class [" + this.targetClass.getName() + "]: " + element);
					}
				}
			}
			Set<LifecycleElement> checkedDestroyMethods = new LinkedHashSet<>(this.destroyMethods.size());
			//2. destroyMethod方法的检查以及设置
			//2.1 遍历检查所有的收集的destroyMethod方法
			for (LifecycleElement element : this.destroyMethods) {
				String methodIdentifier = element.getIdentifier();
				// 2.2如果beanDefinition的externallyManagedConfigMembers属性不包含该methodIdentifier
				if (!beanDefinition.isExternallyManagedDestroyMethod(methodIdentifier)) {
					// 2.3将该methodIdentifier添加到beanDefinition的externallyManagedConfigMembers属性
					beanDefinition.registerExternallyManagedDestroyMethod(methodIdentifier);
					// 2.4并将element添加到checkedElements
					checkedDestroyMethods.add(element);
					if (logger.isTraceEnabled()) {
						logger.trace("Registered destroy method on class [" + this.targetClass.getName() + "]: " + element);
					}
				}
			}
			this.checkedInitMethods = checkedInitMethods;
			this.checkedDestroyMethods = checkedDestroyMethods;
		}

		/**
		 * 调用initMethod
		 *
		 * @param target
		 * @param beanName
		 * @throws Throwable
		 */
		public void invokeInitMethods(Object target, String beanName) throws Throwable {
			//1. 获取搜集的并检查过的checkedInitMethods
			Collection<LifecycleElement> checkedInitMethods = this.checkedInitMethods;
			//2. 如果checkedInitMethods为空就选择initMethods
			Collection<LifecycleElement> initMethodsToIterate =
					(checkedInitMethods != null ? checkedInitMethods : this.initMethods);
			//3. 如果都不为空
			if (!initMethodsToIterate.isEmpty()) {
				//4. 遍历initMethodsToIterate并进行调用
				for (LifecycleElement element : initMethodsToIterate) {
					if (logger.isTraceEnabled()) {
						logger.trace("Invoking init method on bean '" + beanName + "': " + element.getMethod());
					}
					//5. 反射调用
					element.invoke(target);
				}
			}
		}

		/**
		 * 调用销毁方法
		 *
		 * @param target
		 * @param beanName
		 * @throws Throwable
		 */
		public void invokeDestroyMethods(Object target, String beanName) throws Throwable {
			//1. 获取销毁方法对象的列表
			Collection<LifecycleElement> checkedDestroyMethods = this.checkedDestroyMethods;
			//2. 如果checkedDestroyMethods为空就选择destroyMethods
			Collection<LifecycleElement> destroyMethodsToUse =
					(checkedDestroyMethods != null ? checkedDestroyMethods : this.destroyMethods);
			if (!destroyMethodsToUse.isEmpty()) {
				//3. 遍历所有的元素
				for (LifecycleElement element : destroyMethodsToUse) {
					if (logger.isTraceEnabled()) {
						logger.trace("Invoking destroy method on bean '" + beanName + "': " + element.getMethod());
					}
					//4. 反射进行调用
					element.invoke(target);
				}
			}
		}

		public boolean hasDestroyMethods() {
			Collection<LifecycleElement> checkedDestroyMethods = this.checkedDestroyMethods;
			Collection<LifecycleElement> destroyMethodsToUse =
					(checkedDestroyMethods != null ? checkedDestroyMethods : this.destroyMethods);
			return !destroyMethodsToUse.isEmpty();
		}
	}


	/**
	 * Class representing injection information about an annotated method.
	 */
	private static class LifecycleElement {
		//方法
		private final Method method;
		//标识符
		private final String identifier;

		//构造方法，传入Method并生成identifier
		public LifecycleElement(Method method) {
			if (method.getParameterCount() != 0) {
				throw new IllegalStateException("Lifecycle method annotation requires a no-arg method: " + method);
			}
			this.method = method;
			this.identifier = (Modifier.isPrivate(method.getModifiers()) ?
					ClassUtils.getQualifiedMethodName(method) : method.getName());
		}

		public Method getMethod() {
			return this.method;
		}

		public String getIdentifier() {
			return this.identifier;
		}

		//通过反射完成方法的调用
		public void invoke(Object target) throws Throwable {
			ReflectionUtils.makeAccessible(this.method);
			this.method.invoke(target, (Object[]) null);
		}

		@Override
		public boolean equals(@Nullable Object other) {
			if (this == other) {
				return true;
			}
			if (!(other instanceof LifecycleElement)) {
				return false;
			}
			LifecycleElement otherElement = (LifecycleElement) other;
			return (this.identifier.equals(otherElement.identifier));
		}

		@Override
		public int hashCode() {
			return this.identifier.hashCode();
		}
	}

}
