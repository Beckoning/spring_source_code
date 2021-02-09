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

import java.util.Collections;
import java.util.HashMap;
import java.util.HashSet;
import java.util.Iterator;
import java.util.LinkedHashMap;
import java.util.LinkedHashSet;
import java.util.Map;
import java.util.Set;
import java.util.concurrent.ConcurrentHashMap;

import org.springframework.beans.factory.BeanCreationException;
import org.springframework.beans.factory.BeanCreationNotAllowedException;
import org.springframework.beans.factory.BeanCurrentlyInCreationException;
import org.springframework.beans.factory.DisposableBean;
import org.springframework.beans.factory.ObjectFactory;
import org.springframework.beans.factory.config.SingletonBeanRegistry;
import org.springframework.core.SimpleAliasRegistry;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.StringUtils;

/**
 * Generic registry for shared bean instances, implementing the
 * {@link org.springframework.beans.factory.config.SingletonBeanRegistry}.
 * Allows for registering singleton instances that should be shared
 * for all callers of the registry, to be obtained via bean name.
 *
 * <p>Also supports registration of
 * {@link org.springframework.beans.factory.DisposableBean} instances,
 * (which might or might not correspond to registered singletons),
 * to be destroyed on shutdown of the registry. Dependencies between
 * beans can be registered to enforce an appropriate shutdown order.
 *
 * <p>This class mainly serves as base class for
 * {@link org.springframework.beans.factory.BeanFactory} implementations,
 * factoring out the common management of singleton bean instances. Note that
 * the {@link org.springframework.beans.factory.config.ConfigurableBeanFactory}
 * interface extends the {@link SingletonBeanRegistry} interface.
 *
 * <p>Note that this class assumes neither a bean definition concept
 * nor a specific creation process for bean instances, in contrast to
 * {@link AbstractBeanFactory} and {@link DefaultListableBeanFactory}
 * (which inherit from it). Can alternatively also be used as a nested
 * helper to delegate to.
 *
 * @author Juergen Hoeller
 * @see #registerSingleton
 * @see #registerDisposableBean
 * @see org.springframework.beans.factory.DisposableBean
 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory
 * @since 2.0
 */
public class DefaultSingletonBeanRegistry extends SimpleAliasRegistry implements SingletonBeanRegistry {

	/**
	 * Cache of singleton objects: bean name to bean instance.
	 * 单例对象的缓存
	 * 一级缓存
	 * 用户存放完全初始化好的bean 从改缓存取出来的bean可以直接使用
	 */
	private final Map<String, Object> singletonObjects = new ConcurrentHashMap<>(256);

	/**
	 * Cache of singleton factories: bean name to ObjectFactory.
	 * 三级缓存
	 * 存放 bean工厂对象 解决循环依赖
	 */
	private final Map<String, ObjectFactory<?>> singletonFactories = new HashMap<>(16);

	/**
	 * Cache of early singleton objects: bean name to bean instance.
	 * 二级缓存
	 * 存放原始的bean对象   用于解决循环依赖  注意：存在里面的对象还没有被填充属性
	 */
	private final Map<String, Object> earlySingletonObjects = new HashMap<>(16);

	/**
	 * Set of registered singletons, containing the bean names in registration order.
	 */
	private final Set<String> registeredSingletons = new LinkedHashSet<>(256);

	/**
	 * Names of beans that are currently in creation.
	 * 这个集合标志着这个bean是否是正在创建
	 */
	private final Set<String> singletonsCurrentlyInCreation =
			Collections.newSetFromMap(new ConcurrentHashMap<>(16));

	/**
	 * Names of beans currently excluded from in creation checks.
	 * 这个集合保存着需要不需要实例化的bean
	 */
	private final Set<String> inCreationCheckExclusions =
			Collections.newSetFromMap(new ConcurrentHashMap<>(16));

	/**
	 * List of suppressed Exceptions, available for associating related causes.
	 */
	@Nullable
	private Set<Exception> suppressedExceptions;

	/**
	 * Flag that indicates whether we're currently within destroySingletons.
	 */
	private boolean singletonsCurrentlyInDestruction = false;

	/**
	 * Disposable bean instances: bean name to disposable instance.
	 * 注册的销毁的bean
	 */
	private final Map<String, Object> disposableBeans = new LinkedHashMap<>();

	/**
	 * Map between containing bean names: bean name to Set of bean names that the bean contains.
	 */
	private final Map<String, Set<String>> containedBeanMap = new ConcurrentHashMap<>(16);

	/**
	 * Map between dependent bean names: bean name to Set of dependent bean names.
	 * 依赖bean和name的映射关系
	 */
	//通过一个bean名称 可以查询所有依赖自己的bean --- 查询谁依赖的我
	private final Map<String, Set<String>> dependentBeanMap = new ConcurrentHashMap<>(64);

	/**
	 * Map between depending bean names: bean name to Set of bean names for the bean's dependencies.
	 */
	//通过一个bean名称 查询该bean的所有依赖  -- 查询我依赖的谁
	private final Map<String, Set<String>> dependenciesForBeanMap = new ConcurrentHashMap<>(64);


	@Override
	public void registerSingleton(String beanName, Object singletonObject) throws IllegalStateException {
		Assert.notNull(beanName, "Bean name must not be null");
		Assert.notNull(singletonObject, "Singleton object must not be null");
		synchronized (this.singletonObjects) {
			Object oldObject = this.singletonObjects.get(beanName);
			if (oldObject != null) {
				throw new IllegalStateException("Could not register object [" + singletonObject +
						"] under bean name '" + beanName + "': there is already object [" + oldObject + "] bound");
			}
			addSingleton(beanName, singletonObject);
		}
	}

	/**
	 * Add the given singleton object to the singleton cache of this factory.
	 * <p>To be called for eager registration of singletons.
	 * 将实例化完成的单例对象添加到单例对象缓存
	 *
	 * @param beanName        the name of the bean
	 * @param singletonObject the singleton object
	 */
	protected void addSingleton(String beanName, Object singletonObject) {
		synchronized (this.singletonObjects) {
			// 1.添加到单例对象缓存，添加进一级缓存
			this.singletonObjects.put(beanName, singletonObject);
			//2.将单例工厂缓存移除（已经不需要），删除三级缓存
			this.singletonFactories.remove(beanName);
			//3.将早期单例对象缓存移除（已经不需要），删除二级缓存
			this.earlySingletonObjects.remove(beanName);
			// 4.添加到已经注册的单例对象缓存
			this.registeredSingletons.add(beanName);
		}
	}

	/**
	 * Add the given singleton factory for building the specified singleton
	 * if necessary.
	 * <p>To be called for eager registration of singletons, e.g. to be able to
	 * resolve circular references.
	 * 设置三级缓存
	 *
	 * @param beanName         the name of the bean
	 * @param singletonFactory the factory for the singleton object
	 */
	protected void addSingletonFactory(String beanName, ObjectFactory<?> singletonFactory) {
		Assert.notNull(singletonFactory, "Singleton factory must not be null");
		synchronized (this.singletonObjects) {
			// 1.如果beanName不存在于singletonObjects缓存中
			//一级缓存不存在beanName
			if (!this.singletonObjects.containsKey(beanName)) {
				// 2.将beanName和singletonFactory注册到singletonFactories缓存（beanName -> 该beanName的单例工厂）
				//设置beanName和匿名类的映射关系加入到三级缓存
				this.singletonFactories.put(beanName, singletonFactory);
				// 3.移除earlySingletonObjects缓存中的beanName（beanName -> beanName的早期单例对象）
				this.earlySingletonObjects.remove(beanName);
				// 4.将beanName注册到registeredSingletons缓存（已经注册的单例集合）
				this.registeredSingletons.add(beanName);
			}
		}
	}

	@Override
	@Nullable
	public Object getSingleton(String beanName) {
		return getSingleton(beanName, true);
	}

	/**
	 * Return the (raw) singleton object registered under the given name.
	 * <p>Checks already instantiated singletons and also allows for an early
	 * reference to a currently created singleton (resolving a circular reference).
	 * 从缓存中获取单例的bean
	 *
	 * @param beanName            the name of the bean to look for
	 * @param allowEarlyReference whether early references should be created or not
	 * @return the registered singleton object, or {@code null} if none found
	 */
	@Nullable
	protected Object getSingleton(String beanName, boolean allowEarlyReference) {
		//1.从单例对象缓存中获取beanName对应的单例对象
		//先从一级缓存拿
		Object singletonObject = this.singletonObjects.get(beanName);
		//2.如果单例对象缓存中没有，并且该beanName对应的单例bean正在创建中
		//如果bean还正在创建，还没创建完成，其实就是堆内存有了，属性还没有DI依赖注入
		if (singletonObject == null && isSingletonCurrentlyInCreation(beanName)) {
			// 3.加锁进行操作
			synchronized (this.singletonObjects) {
				// 4.从早期单例对象缓存中获取单例对象（之所称成为早期单例对象，是因为earlySingletonObjects里
				//从二级缓存中拿
				singletonObject = this.earlySingletonObjects.get(beanName);
				// 5.如果在早期单例对象缓存中也没有，并且允许创建早期单例对象引用
				//如果还拿不到，并且允许bean提前暴露
				if (singletonObject == null && allowEarlyReference) {
					// 6.从单例工厂缓存中获取beanName的单例工厂
					//从三级缓存中获取
					ObjectFactory<?> singletonFactory = this.singletonFactories.get(beanName);
					if (singletonFactory != null) {
						// 7.如果存在单例对象工厂，则通过工厂创建一个单例对象
						//注意这是一个钩子方法
						singletonObject = singletonFactory.getObject();
						//升级到二级缓存
						// 8.将通过单例对象工厂创建的单例对象，放到早期单例对象缓存中
						this.earlySingletonObjects.put(beanName, singletonObject);
						//删除三级缓存
						// 9.移除该beanName对应的单例对象工厂，因为该单例工厂已经创建了一个实例对象，并且放到earlySingletonObjects缓存了，
						// 因此，后续获取beanName的单例对象，可以通过earlySingletonObjects缓存拿到，不需要在用到该单例工厂
						this.singletonFactories.remove(beanName);
					}
				}
			}
		}
		// 10.返回单例对象
		return singletonObject;
	}

	/**
	 * Return the (raw) singleton object registered under the given name,
	 * creating and registering a new one if none registered yet.
	 * 返回一个实例化的单例对象，如果没有实例化就进行实例化
	 *
	 * @param beanName         the name of the bean
	 * @param singletonFactory the ObjectFactory to lazily create the singleton
	 *                         with, if necessary
	 * @return the registered singleton object
	 */
	public Object getSingleton(String beanName, ObjectFactory<?> singletonFactory) {
		Assert.notNull(beanName, "Bean name must not be null");
		// 1.加锁，避免重复创建单例对象
		synchronized (this.singletonObjects) {
			// 2.首先检查beanName对应的bean实例是否在缓存中存在，如果已经存在，则直接返回
			Object singletonObject = this.singletonObjects.get(beanName);
			//第一次调用必定为空
			// 3.beanName对应的bean实例不存在于缓存中，则进行Bean的创建
			if (singletonObject == null) {
				// 4.当bean工厂的单例处于destruction状态时，不允许进行单例bean创建，抛出异常
				if (this.singletonsCurrentlyInDestruction) {
					throw new BeanCreationNotAllowedException(beanName,
							"Singleton bean creation not allowed while singletons of this factory are in destruction " +
									"(Do not request a bean from a BeanFactory in a destroy method implementation!)");
				}
				if (logger.isDebugEnabled()) {
					logger.debug("Creating shared instance of singleton bean '" + beanName + "'");
				}
				// 5.创建单例前的操作
				//把beanName添加到singletonsCurrentlyInCreation Set容器中，在这个集合里面的bean都是正在实例化的bean
				beforeSingletonCreation(beanName);
				boolean newSingleton = false;
				// suppressedExceptions用于记录异常相关信息
				boolean recordSuppressedExceptions = (this.suppressedExceptions == null);
				if (recordSuppressedExceptions) {
					this.suppressedExceptions = new LinkedHashSet<>();
				}
				try {
					// 6.执行singletonFactory的getObject方法获取bean实例
					//如果这里有返回值，就代表这个bean已经结束创建了，已经完全创建成功
					//调到lamda表达式的createBean(beanName, mbd, args);的方法
					singletonObject = singletonFactory.getObject();
					// 标记为新的单例对象
					newSingleton = true;
				} catch (IllegalStateException ex) {
					// Has the singleton object implicitly appeared in the meantime ->
					// if yes, proceed with it since the exception indicates that state.
					singletonObject = this.singletonObjects.get(beanName);
					if (singletonObject == null) {
						throw ex;
					}
				} catch (BeanCreationException ex) {
					if (recordSuppressedExceptions) {
						for (Exception suppressedException : this.suppressedExceptions) {
							ex.addRelatedCause(suppressedException);
						}
					}
					throw ex;
				} finally {
					if (recordSuppressedExceptions) {
						this.suppressedExceptions = null;
					}
					// 7.创建单例后的操作
					//bean创建完成后singletonsCurrentlyInCreation要删除该bean
					afterSingletonCreation(beanName);
				}
				if (newSingleton) {
					// 8.如果是新的单例对象，将beanName和对应的bean实例添加到缓存中（singletonObjects、registeredSingletons）
					//创建对象成功时，把对象缓存到singletonObjects缓存中,bean创建完成时放入一级缓存
					addSingleton(beanName, singletonObject);
				}
			}
			// 9.返回创建出来的单例对象
			return singletonObject;
		}
	}

	/**
	 * Register an Exception that happened to get suppressed during the creation of a
	 * singleton bean instance, e.g. a temporary circular reference resolution problem.
	 *
	 * @param ex the Exception to register
	 */
	protected void onSuppressedException(Exception ex) {
		synchronized (this.singletonObjects) {
			if (this.suppressedExceptions != null) {
				this.suppressedExceptions.add(ex);
			}
		}
	}

	/**
	 * Remove the bean with the given name from the singleton cache of this factory,
	 * to be able to clean up eager registration of a singleton if creation failed.
	 *
	 * @param beanName the name of the bean
	 * @see #getSingletonMutex()
	 */
	protected void removeSingleton(String beanName) {
		synchronized (this.singletonObjects) {
			this.singletonObjects.remove(beanName);
			this.singletonFactories.remove(beanName);
			this.earlySingletonObjects.remove(beanName);
			this.registeredSingletons.remove(beanName);
		}
	}

	/**
	 * singletonObjects的缓存中是否存在beanName
	 *
	 * @param beanName
	 * @return
	 */
	@Override
	public boolean containsSingleton(String beanName) {
		return this.singletonObjects.containsKey(beanName);
	}

	@Override
	public String[] getSingletonNames() {
		synchronized (this.singletonObjects) {
			return StringUtils.toStringArray(this.registeredSingletons);
		}
	}

	@Override
	public int getSingletonCount() {
		synchronized (this.singletonObjects) {
			return this.registeredSingletons.size();
		}
	}


	public void setCurrentlyInCreation(String beanName, boolean inCreation) {
		Assert.notNull(beanName, "Bean name must not be null");
		if (!inCreation) {
			this.inCreationCheckExclusions.add(beanName);
		} else {
			this.inCreationCheckExclusions.remove(beanName);
		}
	}

	public boolean isCurrentlyInCreation(String beanName) {
		Assert.notNull(beanName, "Bean name must not be null");
		return (!this.inCreationCheckExclusions.contains(beanName) && isActuallyInCreation(beanName));
	}

	protected boolean isActuallyInCreation(String beanName) {
		return isSingletonCurrentlyInCreation(beanName);
	}

	/**
	 * Return whether the specified singleton bean is currently in creation
	 * (within the entire factory).
	 * 返回beanName是否正在创建
	 *
	 * @param beanName the name of the bean
	 */
	public boolean isSingletonCurrentlyInCreation(String beanName) {
		return this.singletonsCurrentlyInCreation.contains(beanName);
	}

	/**
	 * Callback before singleton creation.
	 * <p>The default implementation register the singleton as currently in creation.
	 * 添加正在实例化的bean
	 * 如果再次添加一个存在的就会直接报错
	 * 标识正在创建的bean
	 *
	 * @param beanName the name of the singleton about to be created
	 * @see #isSingletonCurrentlyInCreation
	 */
	protected void beforeSingletonCreation(String beanName) {
		// 把beanName添加到singletonsCurrentlyInCreation Set容器中，在这个集合里面的bean都是正在实例化的bean
		// 先校验beanName是否为要在创建检查排除掉的（inCreationCheckExclusions缓存），如果不是， inCreationCheckExclusions 返回true表示在当前检查需要排除 返回false表示不需要排除
		// 则将beanName加入到正在创建bean的缓存中（Set），如果beanName已经存在于该缓存，会返回false抛出异常（这种情况出现在构造器的循环依赖）
		if (!this.inCreationCheckExclusions.contains(beanName) && !this.singletonsCurrentlyInCreation.add(beanName)) {
			throw new BeanCurrentlyInCreationException(beanName);
		}
	}

	/**
	 * Callback after singleton creation.
	 * <p>The default implementation marks the singleton as not in creation anymore.
	 * 完成实例化从inCreationCheckExclusions中删除实例化完成的beanName
	 *
	 * @param beanName the name of the singleton that has been created
	 * @see #isSingletonCurrentlyInCreation
	 */
	protected void afterSingletonCreation(String beanName) {
		// 先校验beanName是否为要在创建检查排除掉的（inCreationCheckExclusions缓存），如果不是，
		// 则将beanName从正在创建bean的缓存中（Set）移除，如果beanName不存在于该缓存，会返回false抛出异常
		if (!this.inCreationCheckExclusions.contains(beanName) && !this.singletonsCurrentlyInCreation.remove(beanName)) {
			throw new IllegalStateException("Singleton '" + beanName + "' isn't currently in creation");
		}
	}


	/**
	 * Add the given bean to the list of disposable beans in this registry.
	 * <p>Disposable beans usually correspond to registered singletons,
	 * matching the bean name but potentially being a different instance
	 * (for example, a DisposableBean adapter for a singleton that does not
	 * naturally implement Spring's DisposableBean interface).
	 * 注册销毁的bean
	 *
	 * @param beanName the name of the bean
	 * @param bean     the bean instance
	 */
	public void registerDisposableBean(String beanName, DisposableBean bean) {
		synchronized (this.disposableBeans) {
			this.disposableBeans.put(beanName, bean);
		}
	}

	/**
	 * Register a containment relationship between two beans,
	 * e.g. between an inner bean and its containing outer bean.
	 * <p>Also registers the containing bean as dependent on the contained bean
	 * in terms of destruction order.
	 *
	 * @param containedBeanName  the name of the contained (inner) bean
	 * @param containingBeanName the name of the containing (outer) bean
	 * @see #registerDependentBean
	 */
	public void registerContainedBean(String containedBeanName, String containingBeanName) {
		synchronized (this.containedBeanMap) {
			Set<String> containedBeans =
					this.containedBeanMap.computeIfAbsent(containingBeanName, k -> new LinkedHashSet<>(8));
			if (!containedBeans.add(containedBeanName)) {
				return;
			}
		}
		registerDependentBean(containedBeanName, containingBeanName);
	}

	/**
	 * Register a dependent bean for the given bean,
	 * to be destroyed before the given bean is destroyed.
	 * 注册依赖的bean和bean name的映射关系
	 *
	 * @param beanName          the name of the bean
	 * @param dependentBeanName the name of the dependent bean
	 */
	public void registerDependentBean(String beanName, String dependentBeanName) {
		// 1.解析别名
		String canonicalName = canonicalName(beanName);

		synchronized (this.dependentBeanMap) {
			//2. 拿到依赖canonicalName的beanName集合
			//3.将dependentBeanName添加到依赖canonicalName的beanName集合中
			Set<String> dependentBeans =
					this.dependentBeanMap.computeIfAbsent(canonicalName, k -> new LinkedHashSet<>(8));
			// 5.如果dependentBeans包含dependentBeanName，则表示依赖关系已经存在，直接返回
			if (!dependentBeans.add(dependentBeanName)) {
				return;
			}
		}
		//4.如果依赖关系还没有注册，则将两者的关系注册到dependentBeanMap和dependenciesForBeanMap缓存
		synchronized (this.dependenciesForBeanMap) {
			Set<String> dependenciesForBean =
					this.dependenciesForBeanMap.computeIfAbsent(dependentBeanName, k -> new LinkedHashSet<>(8));
			dependenciesForBean.add(canonicalName);
		}
	}

	/**
	 * Determine whether the specified dependent bean has been registered as
	 * dependent on the given bean or on any of its transitive dependencies.
	 * 循环依赖检查
	 *
	 * @param beanName          the name of the bean to check
	 * @param dependentBeanName the name of the dependent bean
	 * @since 4.0
	 */
	protected boolean isDependent(String beanName, String dependentBeanName) {
		synchronized (this.dependentBeanMap) {
			return isDependent(beanName, dependentBeanName, null);
		}
	}

	/**
	 * 循环依赖检查
	 *
	 * @param beanName
	 * @param dependentBeanName
	 * @param alreadySeen
	 * @return
	 */
	private boolean isDependent(String beanName, String dependentBeanName, @Nullable Set<String> alreadySeen) {
		// 已经检查过的直接跳过
		if (alreadySeen != null && alreadySeen.contains(beanName)) {
			return false;
		}
		// 1.将别名解析为真正的名称
		String canonicalName = canonicalName(beanName);
		// 2.拿到依赖canonicalName的beanName集合
		Set<String> dependentBeans = this.dependentBeanMap.get(canonicalName);
		// 3.如果dependentBeans为空，则两者必然还未确定依赖关系，返回false
		if (dependentBeans == null) {
			return false;
		}
		// 4.如果dependentBeans包含dependentBeanName，则表示两者已确定依赖关系，返回true
		if (dependentBeans.contains(dependentBeanName)) {
			return true;
		}
		// 5.循环检查，即检查依赖canonicalName的所有beanName是否存在被dependentBeanName依赖的（即隔层依赖）
		for (String transitiveDependency : dependentBeans) {
			if (alreadySeen == null) {
				alreadySeen = new HashSet<>();
			}
			//加入到set
			alreadySeen.add(beanName);
			if (isDependent(transitiveDependency, dependentBeanName, alreadySeen)) {
				return true;
			}
		}
		return false;
	}

	/**
	 * Determine whether a dependent bean has been registered for the given name.
	 *
	 * @param beanName the name of the bean to check
	 */
	protected boolean hasDependentBean(String beanName) {
		return this.dependentBeanMap.containsKey(beanName);
	}

	/**
	 * Return the names of all beans which depend on the specified bean, if any.
	 *
	 * @param beanName the name of the bean
	 * @return the array of dependent bean names, or an empty array if none
	 */
	public String[] getDependentBeans(String beanName) {
		Set<String> dependentBeans = this.dependentBeanMap.get(beanName);
		if (dependentBeans == null) {
			return new String[0];
		}
		synchronized (this.dependentBeanMap) {
			return StringUtils.toStringArray(dependentBeans);
		}
	}

	/**
	 * Return the names of all beans that the specified bean depends on, if any.
	 *
	 * @param beanName the name of the bean
	 * @return the array of names of beans which the bean depends on,
	 * or an empty array if none
	 */
	public String[] getDependenciesForBean(String beanName) {
		Set<String> dependenciesForBean = this.dependenciesForBeanMap.get(beanName);
		if (dependenciesForBean == null) {
			return new String[0];
		}
		synchronized (this.dependenciesForBeanMap) {
			return StringUtils.toStringArray(dependenciesForBean);
		}
	}

	public void destroySingletons() {
		if (logger.isTraceEnabled()) {
			logger.trace("Destroying singletons in " + this);
		}
		synchronized (this.singletonObjects) {
			this.singletonsCurrentlyInDestruction = true;
		}

		String[] disposableBeanNames;
		synchronized (this.disposableBeans) {
			disposableBeanNames = StringUtils.toStringArray(this.disposableBeans.keySet());
		}
		for (int i = disposableBeanNames.length - 1; i >= 0; i--) {
			destroySingleton(disposableBeanNames[i]);
		}

		this.containedBeanMap.clear();
		this.dependentBeanMap.clear();
		this.dependenciesForBeanMap.clear();

		clearSingletonCache();
	}

	/**
	 * Clear all cached singleton instances in this registry.
	 *
	 * @since 4.3.15
	 */
	protected void clearSingletonCache() {
		synchronized (this.singletonObjects) {
			this.singletonObjects.clear();
			this.singletonFactories.clear();
			this.earlySingletonObjects.clear();
			this.registeredSingletons.clear();
			this.singletonsCurrentlyInDestruction = false;
		}
	}

	/**
	 * Destroy the given bean. Delegates to {@code destroyBean}
	 * if a corresponding disposable bean instance is found.
	 *
	 * @param beanName the name of the bean
	 * @see #destroyBean
	 */
	public void destroySingleton(String beanName) {
		// Remove a registered singleton of the given name, if any.
		removeSingleton(beanName);

		// Destroy the corresponding DisposableBean instance.
		DisposableBean disposableBean;
		synchronized (this.disposableBeans) {
			disposableBean = (DisposableBean) this.disposableBeans.remove(beanName);
		}
		destroyBean(beanName, disposableBean);
	}

	/**
	 * Destroy the given bean. Must destroy beans that depend on the given
	 * bean before the bean itself. Should not throw any exceptions.
	 *
	 * @param beanName the name of the bean
	 * @param bean     the bean instance to destroy
	 */
	protected void destroyBean(String beanName, @Nullable DisposableBean bean) {
		// Trigger destruction of dependent beans first...
		Set<String> dependencies;
		synchronized (this.dependentBeanMap) {
			// Within full synchronization in order to guarantee a disconnected Set
			dependencies = this.dependentBeanMap.remove(beanName);
		}
		if (dependencies != null) {
			if (logger.isTraceEnabled()) {
				logger.trace("Retrieved dependent beans for bean '" + beanName + "': " + dependencies);
			}
			for (String dependentBeanName : dependencies) {
				destroySingleton(dependentBeanName);
			}
		}

		// Actually destroy the bean now...
		if (bean != null) {
			try {
				bean.destroy();
			} catch (Throwable ex) {
				if (logger.isWarnEnabled()) {
					logger.warn("Destruction of bean with name '" + beanName + "' threw an exception", ex);
				}
			}
		}

		// Trigger destruction of contained beans...
		Set<String> containedBeans;
		synchronized (this.containedBeanMap) {
			// Within full synchronization in order to guarantee a disconnected Set
			containedBeans = this.containedBeanMap.remove(beanName);
		}
		if (containedBeans != null) {
			for (String containedBeanName : containedBeans) {
				destroySingleton(containedBeanName);
			}
		}

		// Remove destroyed bean from other beans' dependencies.
		synchronized (this.dependentBeanMap) {
			for (Iterator<Map.Entry<String, Set<String>>> it = this.dependentBeanMap.entrySet().iterator(); it.hasNext(); ) {
				Map.Entry<String, Set<String>> entry = it.next();
				Set<String> dependenciesToClean = entry.getValue();
				dependenciesToClean.remove(beanName);
				if (dependenciesToClean.isEmpty()) {
					it.remove();
				}
			}
		}

		// Remove destroyed bean's prepared dependency information.
		this.dependenciesForBeanMap.remove(beanName);
	}

	/**
	 * Exposes the singleton mutex to subclasses and external collaborators.
	 * <p>Subclasses should synchronize on the given Object if they perform
	 * any sort of extended singleton creation phase. In particular, subclasses
	 * should <i>not</i> have their own mutexes involved in singleton creation,
	 * to avoid the potential for deadlocks in lazy-init situations.
	 */
	@Override
	public final Object getSingletonMutex() {
		return this.singletonObjects;
	}

}
