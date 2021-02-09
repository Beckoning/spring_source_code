/*
 * Copyright 2002-2018 the original author or authors.
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

import java.security.AccessControlContext;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.security.PrivilegedActionException;
import java.security.PrivilegedExceptionAction;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.BeansException;
import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.BeanCurrentlyInCreationException;
import org.springframework.beans.factory.FactoryBean;
import org.springframework.beans.factory.FactoryBeanNotInitializedException;
import org.springframework.lang.Nullable;

/**
 * Support base class for singleton registries which need to handle
 * {@link org.springframework.beans.factory.FactoryBean} instances,
 * integrated with {@link DefaultSingletonBeanRegistry}'s singleton management.
 *
 * <p>Serves as base class for {@link AbstractBeanFactory}.
 *
 * @author Juergen Hoeller
 * @since 2.5.1
 */
public abstract class FactoryBeanRegistrySupport extends DefaultSingletonBeanRegistry {

	/**
	 * Cache of singleton objects created by FactoryBeans: FactoryBean name to object.
	 */
	private final Map<String, Object> factoryBeanObjectCache = new ConcurrentHashMap<>(16);


	/**
	 * Determine the type for the given FactoryBean.
	 *
	 * @param factoryBean the FactoryBean instance to check
	 * @return the FactoryBean's object type,
	 * or {@code null} if the type cannot be determined yet
	 */
	@Nullable
	protected Class<?> getTypeForFactoryBean(final FactoryBean<?> factoryBean) {
		try {
			if (System.getSecurityManager() != null) {
				return AccessController.doPrivileged((PrivilegedAction<Class<?>>)
						factoryBean::getObjectType, getAccessControlContext());
			} else {
				return factoryBean.getObjectType();
			}
		} catch (Throwable ex) {
			// Thrown from the FactoryBean's getObjectType implementation.
			logger.info("FactoryBean threw exception from getObjectType, despite the contract saying " +
					"that it should return null if the type of its object cannot be determined yet", ex);
			return null;
		}
	}

	/**
	 * Obtain an object to expose from the given FactoryBean, if available
	 * in cached form. Quick check for minimal synchronization.
	 * 从 factoryBeanObjectCache 缓存中获取实例
	 *
	 * @param beanName the name of the bean
	 * @return the object obtained from the FactoryBean,
	 * or {@code null} if not available
	 */
	@Nullable
	protected Object getCachedObjectForFactoryBean(String beanName) {
		return this.factoryBeanObjectCache.get(beanName);
	}

	/**
	 * Obtain an object to expose from the given FactoryBean.
	 * 从FactoryBean中获取对象
	 *
	 * @param factory           the FactoryBean instance
	 * @param beanName          the name of the bean
	 * @param shouldPostProcess whether the bean is subject to post-processing
	 * @return the object obtained from the FactoryBean
	 * @throws BeanCreationException if FactoryBean object creation failed
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	protected Object getObjectFromFactoryBean(FactoryBean<?> factory, String beanName, boolean shouldPostProcess) {
		// 1.如果是单例，并且已经存在于单例对象缓存中
		//工厂bean是单例的并且singletonObjects的缓存中存在beanName
		if (factory.isSingleton() && containsSingleton(beanName)) {
			synchronized (getSingletonMutex()) {
				// 2.从FactoryBean创建的单例对象的缓存中获取该bean实例
				Object object = this.factoryBeanObjectCache.get(beanName);
				//如果为null
				if (object == null) {
					// 3.调用FactoryBean的getObject方法获取对象实例
					object = doGetObjectFromFactoryBean(factory, beanName);
					// Only post-process and store if not put there already during getObject() call above
					// (e.g. because of circular reference processing triggered by custom getBean calls)
					//4. 再次检查factoryBeanObjectCache是否存在
					Object alreadyThere = this.factoryBeanObjectCache.get(beanName);
					// 5.如果该beanName已经在缓存中存在，则将object替换成缓存中的
					if (alreadyThere != null) {
						object = alreadyThere;
					} else {
						if (shouldPostProcess) {
							//6. 检查beanName是否正在创建如果正在创建直接返回正在创建的实例
							if (isSingletonCurrentlyInCreation(beanName)) {
								// Temporarily return non-post-processed object, not storing it yet..
								return object;
							}
							//7. 将正在创建的Bean加入到inCreationCheckExclusions中
							//在这个集合里面的bean都是正在实例化的bean
							//如果重复添加就会报错，防止构造方法循环依赖
							beforeSingletonCreation(beanName);
							try {
								//提前暴露bean
								//8. 对bean实例进行后置处理，执行所有已注册的BeanPostProcessor的postProcessAfterInitialization方法
								object = postProcessObjectFromFactoryBean(object, beanName);
							} catch (Throwable ex) {
								throw new BeanCreationException(beanName,
										"Post-processing of FactoryBean's singleton object failed", ex);
							} finally {
								//9. Bean创建完成从inCreationCheckExclusions中删除beanName
								afterSingletonCreation(beanName);
							}
						}
						//singletonObjects的缓存中存在beanName
						if (containsSingleton(beanName)) {
							//10. 把实例缓存到factoryBeanObjectCache map中，这个是单独缓存FactoryBean类型实例的map
							this.factoryBeanObjectCache.put(beanName, object);
						}
					}
				}
				//11. 返回object对象实例
				return object;
			}
		} else {
			//缓存中不存在bean
			//12.调用FactoryBean的getObject方法获取对象实例
			Object object = doGetObjectFromFactoryBean(factory, beanName);
			if (shouldPostProcess) {
				try {
					//提前暴漏bean
					//13. 对bean实例进行后置处理，执行所有已注册的BeanPostProcessor的postProcessAfterInitialization方法
					object = postProcessObjectFromFactoryBean(object, beanName);
				} catch (Throwable ex) {
					throw new BeanCreationException(beanName, "Post-processing of FactoryBean's object failed", ex);
				}
			}
			//14. 返回bean的实例
			return object;
		}
	}

	/**
	 * Obtain an object to expose from the given FactoryBean.
	 * 调用getObject获取bean实例
	 *
	 * @param factory  the FactoryBean instance
	 * @param beanName the name of the bean
	 * @return the object obtained from the FactoryBean
	 * @throws BeanCreationException if FactoryBean object creation failed
	 * @see org.springframework.beans.factory.FactoryBean#getObject()
	 */
	private Object doGetObjectFromFactoryBean(final FactoryBean<?> factory, final String beanName)
			throws BeanCreationException {

		Object object;
		// 1.调用FactoryBean的getObject方法获取bean对象实例
		try {
			if (System.getSecurityManager() != null) {
				AccessControlContext acc = getAccessControlContext();
				try {
					// 1.1 带有权限验证的
					object = AccessController.doPrivileged((PrivilegedExceptionAction<Object>) factory::getObject, acc);
				} catch (PrivilegedActionException pae) {
					throw pae.getException();
				}
			} else {
				// 1.2 不带权限
				object = factory.getObject();
			}
		} catch (FactoryBeanNotInitializedException ex) {
			throw new BeanCurrentlyInCreationException(beanName, ex.toString());
		} catch (Throwable ex) {
			throw new BeanCreationException(beanName, "FactoryBean threw exception on object creation", ex);
		}

		// Do not accept a null value for a FactoryBean that's not fully
		// initialized yet: Many FactoryBeans just return null then.
		// 2.getObject返回的是空值，并且该FactoryBean正在初始化中，则直接抛异常，不接受一个尚未完全初始化的FactoryBean的getObject返回的空值
		if (object == null) {
			//2.1 如果该Bean 正在实例化说明出现了循环依赖直接报错
			if (isSingletonCurrentlyInCreation(beanName)) {
				throw new BeanCurrentlyInCreationException(
						beanName, "FactoryBean which is currently in creation returned null from getObject");
			}
			//2.2 如果第一次实例化为空 转换为NullBean
			object = new NullBean();
		}
		//3.返回创建好的bean对象实例
		return object;
	}

	/**
	 * Post-process the given object that has been obtained from the FactoryBean.
	 * The resulting object will get exposed for bean references.
	 * <p>The default implementation simply returns the given object as-is.
	 * Subclasses may override this, for example, to apply post-processors.
	 * 提前暴漏bean
	 *
	 * @param object   the object obtained from the FactoryBean.
	 * @param beanName the name of the bean
	 * @return the object to expose
	 * @throws org.springframework.beans.BeansException if any post-processing failed
	 */
	protected Object postProcessObjectFromFactoryBean(Object object, String beanName) throws BeansException {
		return object;
	}

	/**
	 * Get a FactoryBean for the given bean if possible.
	 *
	 * @param beanName     the name of the bean
	 * @param beanInstance the corresponding bean instance
	 * @return the bean instance as FactoryBean
	 * @throws BeansException if the given bean cannot be exposed as a FactoryBean
	 */
	protected FactoryBean<?> getFactoryBean(String beanName, Object beanInstance) throws BeansException {
		if (!(beanInstance instanceof FactoryBean)) {
			throw new BeanCreationException(beanName,
					"Bean instance of type [" + beanInstance.getClass() + "] is not a FactoryBean");
		}
		return (FactoryBean<?>) beanInstance;
	}

	/**
	 * Overridden to clear the FactoryBean object cache as well.
	 */
	@Override
	protected void removeSingleton(String beanName) {
		synchronized (getSingletonMutex()) {
			super.removeSingleton(beanName);
			this.factoryBeanObjectCache.remove(beanName);
		}
	}

	/**
	 * Overridden to clear the FactoryBean object cache as well.
	 */
	@Override
	protected void clearSingletonCache() {
		synchronized (getSingletonMutex()) {
			super.clearSingletonCache();
			this.factoryBeanObjectCache.clear();
		}
	}

	/**
	 * Return the security context for this bean factory. If a security manager
	 * is set, interaction with the user code will be executed using the privileged
	 * of the security context returned by this method.
	 *
	 * @see AccessController#getContext()
	 */
	protected AccessControlContext getAccessControlContext() {
		return AccessController.getContext();
	}

}
