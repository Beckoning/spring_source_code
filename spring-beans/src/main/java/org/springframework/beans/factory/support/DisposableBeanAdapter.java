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

package org.springframework.beans.factory.support;

import java.io.Serializable;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.ArrayList;
import java.util.List;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.springframework.beans.BeanUtils;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.DestructionAwareBeanPostProcessor;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.CollectionUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

/**
 * Adapter that implements the {@link DisposableBean} and {@link Runnable}
 * interfaces performing various destruction steps on a given bean instance:
 * <ul>
 * <li>DestructionAwareBeanPostProcessors;
 * <li>the bean implementing DisposableBean itself;
 * <li>a custom destroy method specified on the bean definition.
 * </ul>
 *
 * @author Juergen Hoeller
 * @author Costin Leau
 * @author Stephane Nicoll
 * @see AbstractBeanFactory
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.config.DestructionAwareBeanPostProcessor
 * @see AbstractBeanDefinition#getDestroyMethodName()
 * @since 2.0
 */
@SuppressWarnings("serial")
class DisposableBeanAdapter implements DisposableBean, Runnable, Serializable {

	private static final String CLOSE_METHOD_NAME = "close";

	private static final String SHUTDOWN_METHOD_NAME = "shutdown";

	private static final Log logger = LogFactory.getLog(DisposableBeanAdapter.class);


	private final Object bean;

	private final String beanName;

	private final boolean invokeDisposableBean;

	private final boolean nonPublicAccessAllowed;

	@Nullable
	private final AccessControlContext acc;

	@Nullable
	private String destroyMethodName;

	@Nullable
	private transient Method destroyMethod;

	@Nullable
	private List<DestructionAwareBeanPostProcessor> beanPostProcessors;


	/**
	 * Create a new DisposableBeanAdapter for the given bean.
	 * 创建一个新的DisposableBeanAdapter
	 *
	 * @param bean           the bean instance (never {@code null})
	 * @param beanName       the name of the bean
	 * @param beanDefinition the merged bean definition
	 * @param postProcessors the List of BeanPostProcessors
	 *                       (potentially DestructionAwareBeanPostProcessor), if any
	 */
	public DisposableBeanAdapter(Object bean, String beanName, RootBeanDefinition beanDefinition,
								 List<BeanPostProcessor> postProcessors, @Nullable AccessControlContext acc) {

		Assert.notNull(bean, "Disposable bean must not be null");
		this.bean = bean;
		this.beanName = beanName;
		//如果bean实现了DisposableBean接口
		// 1.判断bean是否需要调用DisposableBean的destroy方法
		this.invokeDisposableBean =
				(this.bean instanceof DisposableBean && !beanDefinition.isExternallyManagedDestroyMethod("destroy"));
		this.nonPublicAccessAllowed = beanDefinition.isNonPublicAccessAllowed();
		this.acc = acc;
		// 2.拿到自定义的destroy方法名
		String destroyMethodName = inferDestroyMethodIfNecessary(bean, beanDefinition);
		if (destroyMethodName != null && !(this.invokeDisposableBean && "destroy".equals(destroyMethodName)) &&
				!beanDefinition.isExternallyManagedDestroyMethod(destroyMethodName)) {
			this.destroyMethodName = destroyMethodName;
			// 3.拿到自定义的destroy方法，赋值给this.destroyMethod
			Method destroyMethod = determineDestroyMethod(destroyMethodName);
			if (destroyMethod == null) {
				if (beanDefinition.isEnforceDestroyMethod()) {
					// 4.如果destroy方法名为空，并且enforceDestroyMethod为true，则抛出异常
					throw new BeanDefinitionValidationException("Could not find a destroy method named '" +
							destroyMethodName + "' on bean with name '" + beanName + "'");
				}
			} else {
				// 5.拿到destroy方法的参数类型数组
				//获取销毁方法的参数
				Class<?>[] paramTypes = destroyMethod.getParameterTypes();
				//如果参数>1 直接报错
				// 6.如果destroy方法的参数大于1个，则抛出异常
				if (paramTypes.length > 1) {
					throw new BeanDefinitionValidationException("Method '" + destroyMethodName + "' of bean '" +
							beanName + "' has more than one parameter - not supported as destroy method");
					//如果有一个参数 并且不是boolean类型 直接报错
					// 7.如果destroy方法的参数为1个，并且该参数的类型不为boolean，则抛出异常
				} else if (paramTypes.length == 1 && boolean.class != paramTypes[0]) {
					throw new BeanDefinitionValidationException("Method '" + destroyMethodName + "' of bean '" +
							beanName + "' has a non-boolean parameter - not supported as destroy method");
				}
				//8. 获取该方法的接口方法
				destroyMethod = ClassUtils.getInterfaceMethodIfPossible(destroyMethod);
			}
			//9. 设置到全局的destroyMethod
			this.destroyMethod = destroyMethod;
		}
		//10. 找到支持@Destroy注解的postProcessors
		this.beanPostProcessors = filterPostProcessors(postProcessors, bean);
	}

	/**
	 * Create a new DisposableBeanAdapter for the given bean.
	 *
	 * @param bean           the bean instance (never {@code null})
	 * @param postProcessors the List of BeanPostProcessors
	 *                       (potentially DestructionAwareBeanPostProcessor), if any
	 */
	public DisposableBeanAdapter(Object bean, List<BeanPostProcessor> postProcessors, AccessControlContext acc) {
		Assert.notNull(bean, "Disposable bean must not be null");
		this.bean = bean;
		this.beanName = bean.getClass().getName();
		this.invokeDisposableBean = (this.bean instanceof DisposableBean);
		this.nonPublicAccessAllowed = true;
		this.acc = acc;
		this.beanPostProcessors = filterPostProcessors(postProcessors, bean);
	}

	/**
	 * Create a new DisposableBeanAdapter for the given bean.
	 */
	private DisposableBeanAdapter(Object bean, String beanName, boolean invokeDisposableBean,
								  boolean nonPublicAccessAllowed, @Nullable String destroyMethodName,
								  @Nullable List<DestructionAwareBeanPostProcessor> postProcessors) {

		this.bean = bean;
		this.beanName = beanName;
		this.invokeDisposableBean = invokeDisposableBean;
		this.nonPublicAccessAllowed = nonPublicAccessAllowed;
		this.acc = null;
		this.destroyMethodName = destroyMethodName;
		this.beanPostProcessors = postProcessors;
	}


	/**
	 * If the current value of the given beanDefinition's "destroyMethodName" property is
	 * {@link AbstractBeanDefinition#INFER_METHOD}, then attempt to infer a destroy method.
	 * Candidate methods are currently limited to public, no-arg methods named "close" or
	 * "shutdown" (whether declared locally or inherited). The given BeanDefinition's
	 * "destroyMethodName" is updated to be null if no such method is found, otherwise set
	 * to the name of the inferred method. This constant serves as the default for the
	 * {@code @Bean#destroyMethod} attribute and the value of the constant may also be
	 * used in XML within the {@code <bean destroy-method="">} or {@code
	 * <beans default-destroy-method="">} attributes.
	 * <p>Also processes the {@link java.io.Closeable} and {@link java.lang.AutoCloseable}
	 * interfaces, reflectively calling the "close" method on implementing beans as well.
	 * 获取bean的销毁方法
	 */
	@Nullable
	private String inferDestroyMethodIfNecessary(Object bean, RootBeanDefinition beanDefinition) {
		//1. 从beanDefinition中获取销毁方法
		String destroyMethodName = beanDefinition.getDestroyMethodName();
		// 2.如果destroy方法名为“(inferred)”|| destroyMethodName为null，并且bean是AutoCloseable实例
		//如果destroyMethodName是INFER_METHOD或者为null并且实现了AutoCloseable
		if (AbstractBeanDefinition.INFER_METHOD.equals(destroyMethodName) ||
				(destroyMethodName == null && bean instanceof AutoCloseable)) {
			// Only perform destroy method inference or Closeable detection
			// in case of the bean not explicitly implementing DisposableBean
			// 3.如果bean没有实现DisposableBean接口，则尝试推测destroy方法的名字
			//如果没有实现DisposableBean接口
			if (!(bean instanceof DisposableBean)) {
				try {
					// 4.尝试在bean中寻找方法名为close的方法作为destroy方法
					return bean.getClass().getMethod(CLOSE_METHOD_NAME).getName();
				} catch (NoSuchMethodException ex) {
					try {
						// 5.尝试在bean中寻找方法名为close的方法作为shutdown方法
						return bean.getClass().getMethod(SHUTDOWN_METHOD_NAME).getName();
					} catch (NoSuchMethodException ex2) {
						// no candidate destroy method found
					}
				}
			}
			return null;
		}
		//6. 其他清空返回destroyMethodName方法
		return (StringUtils.hasLength(destroyMethodName) ? destroyMethodName : null);
	}

	/**
	 * Search for all DestructionAwareBeanPostProcessors in the List.
	 * 找到支持@Destroy注解的postProcessors
	 * 找到所有类型是DestructionAwareBeanPostProcessor的BeanPostProcessor 加入到filteredPostProcessors并返回
	 *
	 * @param processors the List to search
	 * @return the filtered List of DestructionAwareBeanPostProcessors
	 */
	@Nullable
	private List<DestructionAwareBeanPostProcessor> filterPostProcessors(List<BeanPostProcessor> processors, Object bean) {
		List<DestructionAwareBeanPostProcessor> filteredPostProcessors = null;
		if (!CollectionUtils.isEmpty(processors)) {
			filteredPostProcessors = new ArrayList<>(processors.size());
			//1. 遍历所有的processors
			for (BeanPostProcessor processor : processors) {
				//2. 如果实现了DestructionAwareBeanPostProcessor接口
				if (processor instanceof DestructionAwareBeanPostProcessor) {
					DestructionAwareBeanPostProcessor dabpp = (DestructionAwareBeanPostProcessor) processor;
					//2.1 检查dabpp是否实现了对应bean的@Destroy注解的支持
					if (dabpp.requiresDestruction(bean)) {
						//2.2 如果给定的bean实例需要通过此后处理器进行销毁，则添加到filteredPostProcessors
						filteredPostProcessors.add(dabpp);
					}
				}
			}
		}
		return filteredPostProcessors;
	}


	@Override
	public void run() {
		destroy();
	}

	/**
	 * 具体的销毁方法
	 */
	@Override
	public void destroy() {
		//如果销毁的beanPostProcessors不为空
		if (!CollectionUtils.isEmpty(this.beanPostProcessors)) {
			for (DestructionAwareBeanPostProcessor processor : this.beanPostProcessors) {
				//调用processor的销毁bean的方法
				processor.postProcessBeforeDestruction(this.bean, this.beanName);
			}
		}
		//如果实现了DisposableBean接口
		if (this.invokeDisposableBean) {
			if (logger.isTraceEnabled()) {
				logger.trace("Invoking destroy() on bean with name '" + this.beanName + "'");
			}
			try {
				if (System.getSecurityManager() != null) {
					AccessController.doPrivileged((PrivilegedExceptionAction<Object>) () -> {
						((DisposableBean) this.bean).destroy();
						return null;
					}, this.acc);
				} else {
					//直接调用destroy方法
					((DisposableBean) this.bean).destroy();
				}
			} catch (Throwable ex) {
				String msg = "Invocation of destroy method failed on bean with name '" + this.beanName + "'";
				if (logger.isDebugEnabled()) {
					logger.warn(msg, ex);
				} else {
					logger.warn(msg + ": " + ex);
				}
			}
		}
		//如果destroyMethod不为空
		if (this.destroyMethod != null) {
			//调用自定义的DestroyMethod
			invokeCustomDestroyMethod(this.destroyMethod);
			//如果destroyMethodName不为空
		} else if (this.destroyMethodName != null) {
			//搜集destroyMethodName方法的method
			Method methodToInvoke = determineDestroyMethod(this.destroyMethodName);
			if (methodToInvoke != null) {
				//调用自定义的DestroyMethod
				invokeCustomDestroyMethod(ClassUtils.getInterfaceMethodIfPossible(methodToInvoke));
			}
		}
	}

	/**
	 * 根据name搜集销毁方法
	 *
	 * @param name
	 * @return
	 */
	@Nullable
	private Method determineDestroyMethod(String name) {
		try {
			if (System.getSecurityManager() != null) {
				return AccessController.doPrivileged((PrivilegedAction<Method>) () -> findDestroyMethod(name));
			} else {
				//返回搜集到的销毁方法
				return findDestroyMethod(name);
			}
		} catch (IllegalArgumentException ex) {
			throw new BeanDefinitionValidationException("Could not find unique destroy method on bean with name '" +
					this.beanName + ": " + ex.getMessage());
		}
	}

	/**
	 * 搜集销毁方法
	 *
	 * @param name
	 * @return
	 */
	@Nullable
	private Method findDestroyMethod(String name) {
		return (this.nonPublicAccessAllowed ?
				BeanUtils.findMethodWithMinimalParameters(this.bean.getClass(), name) :
				BeanUtils.findMethodWithMinimalParameters(this.bean.getClass().getMethods(), name));
	}

	/**
	 * Invoke the specified custom destroy method on the given bean.
	 * <p>This implementation invokes a no-arg method if found, else checking
	 * for a method with a single boolean argument (passing in "true",
	 * assuming a "force" parameter), else logging an error.
	 * 调用自定义的DestroyMethod
	 */
	private void invokeCustomDestroyMethod(final Method destroyMethod) {
		int paramCount = destroyMethod.getParameterCount();
		final Object[] args = new Object[paramCount];
		if (paramCount == 1) {
			args[0] = Boolean.TRUE;
		}
		if (logger.isTraceEnabled()) {
			logger.trace("Invoking destroy method '" + this.destroyMethodName +
					"' on bean with name '" + this.beanName + "'");
		}
		try {
			if (System.getSecurityManager() != null) {
				AccessController.doPrivileged((PrivilegedAction<Object>) () -> {
					ReflectionUtils.makeAccessible(destroyMethod);
					return null;
				});
				try {
					AccessController.doPrivileged((PrivilegedExceptionAction<Object>) () ->
							destroyMethod.invoke(this.bean, args), this.acc);
				} catch (PrivilegedActionException pax) {
					throw (InvocationTargetException) pax.getException();
				}
			} else {
				ReflectionUtils.makeAccessible(destroyMethod);
				destroyMethod.invoke(this.bean, args);
			}
		} catch (InvocationTargetException ex) {
			String msg = "Destroy method '" + this.destroyMethodName + "' on bean with name '" +
					this.beanName + "' threw an exception";
			if (logger.isDebugEnabled()) {
				logger.warn(msg, ex.getTargetException());
			} else {
				logger.warn(msg + ": " + ex.getTargetException());
			}
		} catch (Throwable ex) {
			logger.warn("Failed to invoke destroy method '" + this.destroyMethodName +
					"' on bean with name '" + this.beanName + "'", ex);
		}
	}


	/**
	 * Serializes a copy of the state of this class,
	 * filtering out non-serializable BeanPostProcessors.
	 */
	protected Object writeReplace() {
		List<DestructionAwareBeanPostProcessor> serializablePostProcessors = null;
		if (this.beanPostProcessors != null) {
			serializablePostProcessors = new ArrayList<>();
			for (DestructionAwareBeanPostProcessor postProcessor : this.beanPostProcessors) {
				if (postProcessor instanceof Serializable) {
					serializablePostProcessors.add(postProcessor);
				}
			}
		}
		return new DisposableBeanAdapter(this.bean, this.beanName, this.invokeDisposableBean,
				this.nonPublicAccessAllowed, this.destroyMethodName, serializablePostProcessors);
	}


	/**
	 * Check whether the given bean has any kind of destroy method to call.
	 * 判断 bean 是否有 destroy 方法
	 *
	 * @param bean           the bean instance
	 * @param beanDefinition the corresponding bean definition
	 */
	public static boolean hasDestroyMethod(Object bean, RootBeanDefinition beanDefinition) {
		if (bean instanceof DisposableBean || bean instanceof AutoCloseable) {
			// 1.如果bean实现了DisposableBean接口 或者 bean是AutoCloseable实例，则返回true
			return true;
		}
		// 2.拿到bean自定义的destroy方法名
		String destroyMethodName = beanDefinition.getDestroyMethodName();
		if (AbstractBeanDefinition.INFER_METHOD.equals(destroyMethodName)) {
			// 3.如果自定义的destroy方法名为“(inferred)”（该名字代表需要我们自己去推测destroy的方法名），
			// 则检查该bean是否存在方法名为“close”或“shutdown”的方法，如果存在，则返回true
			return (ClassUtils.hasMethod(bean.getClass(), CLOSE_METHOD_NAME) ||
					ClassUtils.hasMethod(bean.getClass(), SHUTDOWN_METHOD_NAME));
		}
		// 4.如果destroyMethodName不为空，则返回true
		return StringUtils.hasLength(destroyMethodName);
	}

	/**
	 * Check whether the given bean has destruction-aware post-processors applying to it.
	 * 是否存在适用于 bean 的 DestructionAwareBeanPostProcessor
	 *
	 * @param bean           the bean instance
	 * @param postProcessors the post-processor candidates
	 */
	public static boolean hasApplicableProcessors(Object bean, List<BeanPostProcessor> postProcessors) {
		if (!CollectionUtils.isEmpty(postProcessors)) {
			// 1.遍历所有的BeanPostProcessor
			for (BeanPostProcessor processor : postProcessors) {
				// 2.如果processor是DestructionAwareBeanPostProcessor
				if (processor instanceof DestructionAwareBeanPostProcessor) {
					DestructionAwareBeanPostProcessor dabpp = (DestructionAwareBeanPostProcessor) processor;
					if (dabpp.requiresDestruction(bean)) {
						// 3.如果给定的bean实例需要通过此后处理器进行销毁，则返回true
						return true;
					}
				}
			}
		}
		return false;
	}

}
