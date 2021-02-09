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

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;
import org.springframework.beans.BeanUtils;
import org.springframework.beans.BeansException;
import org.springframework.beans.PropertyValues;
import org.springframework.beans.TypeConverter;
import org.springframework.beans.factory.*;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.config.DependencyDescriptor;
import org.springframework.beans.factory.config.InstantiationAwareBeanPostProcessorAdapter;
import org.springframework.beans.factory.support.LookupOverride;
import org.springframework.beans.factory.support.MergedBeanDefinitionPostProcessor;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.core.BridgeMethodResolver;
import org.springframework.core.MethodParameter;
import org.springframework.core.Ordered;
import org.springframework.core.PriorityOrdered;
import org.springframework.core.annotation.AnnotationAttributes;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.annotation.MergedAnnotation;
import org.springframework.core.annotation.MergedAnnotations;
import org.springframework.lang.Nullable;
import org.springframework.util.Assert;
import org.springframework.util.ClassUtils;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;

import java.beans.PropertyDescriptor;
import java.lang.annotation.Annotation;
import java.lang.reflect.*;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;

/**
 * {@link org.springframework.beans.factory.config.BeanPostProcessor BeanPostProcessor}
 * implementation that autowires annotated fields, setter methods, and arbitrary
 * config methods. Such members to be injected are detected through annotations:
 * by default, Spring's {@link Autowired @Autowired} and {@link Value @Value}
 * annotations.
 *
 * <p>Also supports JSR-330's {@link javax.inject.Inject @Inject} annotation,
 * if available, as a direct alternative to Spring's own {@code @Autowired}.
 *
 * <h3>Autowired Constructors</h3>
 * <p>Only one constructor of any given bean class may declare this annotation with
 * the 'required' attribute set to {@code true}, indicating <i>the</i> constructor
 * to autowire when used as a Spring bean. Furthermore, if the 'required' attribute
 * is set to {@code true}, only a single constructor may be annotated with
 * {@code @Autowired}. If multiple <i>non-required</i> constructors declare the
 * annotation, they will be considered as candidates for autowiring. The constructor
 * with the greatest number of dependencies that can be satisfied by matching beans
 * in the Spring container will be chosen. If none of the candidates can be satisfied,
 * then a primary/default constructor (if present) will be used. If a class only
 * declares a single constructor to begin with, it will always be used, even if not
 * annotated. An annotated constructor does not have to be public.
 *
 * <h3>Autowired Fields</h3>
 * <p>Fields are injected right after construction of a bean, before any
 * config methods are invoked. Such a config field does not have to be public.
 *
 * <h3>Autowired Methods</h3>
 * <p>Config methods may have an arbitrary name and any number of arguments; each of
 * those arguments will be autowired with a matching bean in the Spring container.
 * Bean property setter methods are effectively just a special case of such a
 * general config method. Config methods do not have to be public.
 *
 * <h3>Annotation Config vs. XML Config</h3>
 * <p>A default {@code AutowiredAnnotationBeanPostProcessor} will be registered
 * by the "context:annotation-config" and "context:component-scan" XML tags.
 * Remove or turn off the default annotation configuration there if you intend
 * to specify a custom {@code AutowiredAnnotationBeanPostProcessor} bean definition.
 *
 * <p><b>NOTE:</b> Annotation injection will be performed <i>before</i> XML injection;
 * thus the latter configuration will override the former for properties wired through
 * both approaches.
 *
 * <h3>{@literal @}Lookup Methods</h3>
 * <p>In addition to regular injection points as discussed above, this post-processor
 * also handles Spring's {@link Lookup @Lookup} annotation which identifies lookup
 * methods to be replaced by the container at runtime. This is essentially a type-safe
 * version of {@code getBean(Class, args)} and {@code getBean(String, args)}.
 * See {@link Lookup @Lookup's javadoc} for details.
 *
 * @author Juergen Hoeller
 * @author Mark Fisher
 * @author Stephane Nicoll
 * @author Sebastien Deleuze
 * @author Sam Brannen
 * @see #setAutowiredAnnotationType
 * @see Autowired
 * @see Value
 * @since 2.5
 */
public class AutowiredAnnotationBeanPostProcessor extends InstantiationAwareBeanPostProcessorAdapter
		implements MergedBeanDefinitionPostProcessor, PriorityOrdered, BeanFactoryAware {

	protected final Log logger = LogFactory.getLog(getClass());
	/**
	 * 需要进行扫描的注解
	 */
	private final Set<Class<? extends Annotation>> autowiredAnnotationTypes = new LinkedHashSet<>(4);

	private String requiredParameterName = "required";

	private boolean requiredParameterValue = true;

	private int order = Ordered.LOWEST_PRECEDENCE - 2;

	@Nullable
	private ConfigurableListableBeanFactory beanFactory;

	private final Set<String> lookupMethodsChecked = Collections.newSetFromMap(new ConcurrentHashMap<>(256));

	private final Map<Class<?>, Constructor<?>[]> candidateConstructorsCache = new ConcurrentHashMap<>(256);

	private final Map<String, InjectionMetadata> injectionMetadataCache = new ConcurrentHashMap<>(256);


	/**
	 * Create a new {@code AutowiredAnnotationBeanPostProcessor} for Spring's
	 * standard {@link Autowired @Autowired} and {@link Value @Value} annotations.
	 * <p>Also supports JSR-330's {@link javax.inject.Inject @Inject} annotation,
	 * if available.
	 */
	@SuppressWarnings("unchecked")
	public AutowiredAnnotationBeanPostProcessor() {
		this.autowiredAnnotationTypes.add(Autowired.class);
		this.autowiredAnnotationTypes.add(Value.class);
		try {
			this.autowiredAnnotationTypes.add((Class<? extends Annotation>)
					ClassUtils.forName("javax.inject.Inject", AutowiredAnnotationBeanPostProcessor.class.getClassLoader()));
			logger.trace("JSR-330 'javax.inject.Inject' annotation found and supported for autowiring");
		} catch (ClassNotFoundException ex) {
			// JSR-330 API not available - simply skip.
		}
	}


	/**
	 * Set the 'autowired' annotation type, to be used on constructors, fields,
	 * setter methods, and arbitrary config methods.
	 * <p>The default autowired annotation types are the Spring-provided
	 * {@link Autowired @Autowired} and {@link Value @Value} annotations as well
	 * as JSR-330's {@link javax.inject.Inject @Inject} annotation, if available.
	 * <p>This setter property exists so that developers can provide their own
	 * (non-Spring-specific) annotation type to indicate that a member is supposed
	 * to be autowired.
	 */
	public void setAutowiredAnnotationType(Class<? extends Annotation> autowiredAnnotationType) {
		Assert.notNull(autowiredAnnotationType, "'autowiredAnnotationType' must not be null");
		this.autowiredAnnotationTypes.clear();
		this.autowiredAnnotationTypes.add(autowiredAnnotationType);
	}

	/**
	 * Set the 'autowired' annotation types, to be used on constructors, fields,
	 * setter methods, and arbitrary config methods.
	 * <p>The default autowired annotation types are the Spring-provided
	 * {@link Autowired @Autowired} and {@link Value @Value} annotations as well
	 * as JSR-330's {@link javax.inject.Inject @Inject} annotation, if available.
	 * <p>This setter property exists so that developers can provide their own
	 * (non-Spring-specific) annotation types to indicate that a member is supposed
	 * to be autowired.
	 */
	public void setAutowiredAnnotationTypes(Set<Class<? extends Annotation>> autowiredAnnotationTypes) {
		Assert.notEmpty(autowiredAnnotationTypes, "'autowiredAnnotationTypes' must not be empty");
		this.autowiredAnnotationTypes.clear();
		this.autowiredAnnotationTypes.addAll(autowiredAnnotationTypes);
	}

	/**
	 * Set the name of an attribute of the annotation that specifies whether it is required.
	 *
	 * @see #setRequiredParameterValue(boolean)
	 */
	public void setRequiredParameterName(String requiredParameterName) {
		this.requiredParameterName = requiredParameterName;
	}

	/**
	 * Set the boolean value that marks a dependency as required.
	 * <p>For example if using 'required=true' (the default), this value should be
	 * {@code true}; but if using 'optional=false', this value should be {@code false}.
	 *
	 * @see #setRequiredParameterName(String)
	 */
	public void setRequiredParameterValue(boolean requiredParameterValue) {
		this.requiredParameterValue = requiredParameterValue;
	}

	public void setOrder(int order) {
		this.order = order;
	}

	@Override
	public int getOrder() {
		return this.order;
	}

	@Override
	public void setBeanFactory(BeanFactory beanFactory) {
		if (!(beanFactory instanceof ConfigurableListableBeanFactory)) {
			throw new IllegalArgumentException(
					"AutowiredAnnotationBeanPostProcessor requires a ConfigurableListableBeanFactory: " + beanFactory);
		}
		this.beanFactory = (ConfigurableListableBeanFactory) beanFactory;
	}

	/**
	 * 1、扫描类里面的属性或者方法
	 * 2、判断属性或者方法上面是否有@Autowiring注解
	 * 3、如果有注解的属性或者方法，包装成一个类
	 *
	 * @param beanDefinition
	 * @param beanType
	 * @param beanName
	 */
	@Override
	public void postProcessMergedBeanDefinition(RootBeanDefinition beanDefinition, Class<?> beanType, String beanName) {
		//1. 搜集方法以及字段的@Autowired注解并封装成InjectionMetadata
		InjectionMetadata metadata = findAutowiringMetadata(beanName, beanType, null);
		//2. 检测ConfigMembers并将InjectedElement集合赋值给全局的checkedElements
		metadata.checkConfigMembers(beanDefinition);
	}

	@Override
	public void resetBeanDefinition(String beanName) {
		this.lookupMethodsChecked.remove(beanName);
		this.injectionMetadataCache.remove(beanName);
	}

	/**
	 * 查找有@Lookup注解的方法封装成MethodOverride并加入到BeanDefinition的methodOverrides列表
	 * 查找有@utowired注解的构造方法并返回构造方法列表
	 *
	 * @param beanClass
	 * @param beanName
	 * @return
	 * @throws BeanCreationException
	 */
	@Override
	@Nullable
	public Constructor<?>[] determineCandidateConstructors(Class<?> beanClass, final String beanName)
			throws BeanCreationException {

		//查找有@Lookup注解的方法封装成MethodOverride并加入到BeanDefinition的methodOverrides列表
		// Let's check for lookup methods here...
		if (!this.lookupMethodsChecked.contains(beanName)) {
			if (AnnotationUtils.isCandidateClass(beanClass, Lookup.class)) {
				try {
					Class<?> targetClass = beanClass;
					do {
						//对targetClass的所有方法进行回调 封装LookupOverride到BeanDefinition中
						ReflectionUtils.doWithLocalMethods(targetClass, method -> {
							//对注解Lookup的支持 获取Lookup注解
							Lookup lookup = method.getAnnotation(Lookup.class);
							if (lookup != null) {
								Assert.state(this.beanFactory != null, "No BeanFactory available");
								//将method以及lookup封装成LookupOverride
								LookupOverride override = new LookupOverride(method, lookup.value());
								try {
									//获取对应beanName的BeanDefinition
									RootBeanDefinition mbd = (RootBeanDefinition)
											this.beanFactory.getMergedBeanDefinition(beanName);
									//将LookupOverride添加到BeanDefinition中
									mbd.getMethodOverrides().addOverride(override);
								} catch (NoSuchBeanDefinitionException ex) {
									throw new BeanCreationException(beanName,
											"Cannot apply @Lookup to beans without corresponding bean definition");
								}
							}
						});
						targetClass = targetClass.getSuperclass();
					}
					while (targetClass != null && targetClass != Object.class);

				} catch (IllegalStateException ex) {
					throw new BeanCreationException(beanName, "Lookup method resolution failed", ex);
				}
			}
			this.lookupMethodsChecked.add(beanName);
		}

		//从缓存中获取构造函数数组
		// 1.构造函数解析，首先检查是否存在于缓存中
		// Quick check on the concurrent map first, with minimal locking.
		Constructor<?>[] candidateConstructors = this.candidateConstructorsCache.get(beanClass);
		if (candidateConstructors == null) {
			// Fully synchronized resolution now...
			// 2.加锁进行操作
			synchronized (this.candidateConstructorsCache) {
				//再次从缓存中获取造函数数组 为了线程安全考虑
				// 3.再次检查缓存，双重检测
				candidateConstructors = this.candidateConstructorsCache.get(beanClass);
				if (candidateConstructors == null) {
					// 存放原始的构造函数（候选者）
					Constructor<?>[] rawCandidates;
					try {
						//获取bean对应的所有构造器
						rawCandidates = beanClass.getDeclaredConstructors();
					} catch (Throwable ex) {
						throw new BeanCreationException(beanName,
								"Resolution of declared constructors on bean Class [" + beanClass.getName() +
										"] from ClassLoader [" + beanClass.getClassLoader() + "] failed", ex);
					}
					// 存放使用了@Autowire注解的构造函数
					List<Constructor<?>> candidates = new ArrayList<>(rawCandidates.length);
					// 存放使用了@Autowire注解，并且require=true的构造函数
					Constructor<?> requiredConstructor = null;
					// 存放默认的构造函数
					Constructor<?> defaultConstructor = null;
					Constructor<?> primaryConstructor = BeanUtils.findPrimaryConstructor(beanClass);
					int nonSyntheticConstructors = 0;
					// 5.遍历原始的构造函数候选者
					for (Constructor<?> candidate : rawCandidates) {
						if (!candidate.isSynthetic()) {
							nonSyntheticConstructors++;
						} else if (primaryConstructor != null) {
							continue;
						}
						// 6.获取候选者的注解属性
						//获取到构造函数上的@Autowired注解信息,这个方法可以不看
						MergedAnnotation<?> ann = findAutowiredAnnotation(candidate);
						//如果没有找到Autowired注解
						if (ann == null) {
							// 7.如果没有从候选者找到注解，则尝试解析beanClass的原始类（针对CGLIB代理）
							Class<?> userClass = ClassUtils.getUserClass(beanClass);
							if (userClass != beanClass) {
								try {
									//根据参数获取构造函数
									Constructor<?> superCtor =
											userClass.getDeclaredConstructor(candidate.getParameterTypes());
									//检查构造方法是否有Autowired注解
									ann = findAutowiredAnnotation(superCtor);
								} catch (NoSuchMethodException ex) {
									// Simply proceed, no equivalent superclass constructor found...
								}
							}
						}
						//如果找到Autowired注解的构造函数
						// 8.如果该候选者使用了@Autowire注解
						if (ann != null) {
							if (requiredConstructor != null) {
								// 8.1 之前已经存在使用@Autowired(required = true)的构造函数，则不能存在其他使用@Autowire注解的构造函数，否则抛异常
								throw new BeanCreationException(beanName,
										"Invalid autowire-marked constructor: " + candidate +
												". Found constructor with 'required' Autowired annotation already: " +
												requiredConstructor);
							}
							//获取到@Autowired里面的required方法的值
							// 8.2 获取注解的require属性值
							boolean required = determineRequiredStatus(ann);
							if (required) {
								if (!candidates.isEmpty()) {
									// 8.3 如果当前候选者是@Autowired(required = true)，则之前不能存在其他使用@Autowire注解的构造函数，否则抛异常
									throw new BeanCreationException(beanName,
											"Invalid autowire-marked constructors: " + candidates +
													". Found constructor with 'required' Autowired annotation: " +
													candidate);
								}
								// 8.4 如果该候选者使用的注解的required属性为true，赋值给requiredConstructor
								requiredConstructor = candidate;
							}
							// 8.5 将使用了@Autowire注解的候选者添加到candidates
							candidates.add(candidate);
						} else if (candidate.getParameterCount() == 0) {
							// 8.6 如果没有使用注解，并且没有参数，则为默认的构造函数
							defaultConstructor = candidate;
						}
					}
					// 9.如果存在使用了@Autowire注解的构造函数
					if (!candidates.isEmpty()) {
						// Add default constructor to list of optional constructors, as fallback.
						// 9.1 但是没有使用了@Autowire注解并且required属性为true的构造函数
						if (requiredConstructor == null) {
							if (defaultConstructor != null) {
								// 9.2 如果存在默认的构造函数，则将默认的构造函数添加到candidates
								candidates.add(defaultConstructor);
							} else if (candidates.size() == 1 && logger.isInfoEnabled()) {
								logger.info("Inconsistent constructor declaration on bean with name '" + beanName +
										"': single autowire-marked constructor flagged as optional - " +
										"this constructor is effectively required since there is no " +
										"default constructor to fall back to: " + candidates.get(0));
							}
						}
						// 9.3 将所有的candidates当作候选者
						candidateConstructors = candidates.toArray(new Constructor<?>[0]);
					} else if (rawCandidates.length == 1 && rawCandidates[0].getParameterCount() > 0) {
						// 10.如果candidates为空 && beanClass只有一个声明的构造函数（非默认构造函数），则将该声明的构造函数作为候选者
						candidateConstructors = new Constructor<?>[]{rawCandidates[0]};
					} else if (nonSyntheticConstructors == 2 && primaryConstructor != null &&
							defaultConstructor != null && !primaryConstructor.equals(defaultConstructor)) {
						candidateConstructors = new Constructor<?>[]{primaryConstructor, defaultConstructor};
					} else if (nonSyntheticConstructors == 1 && primaryConstructor != null) {
						candidateConstructors = new Constructor<?>[]{primaryConstructor};
					} else {
						// 11.否则返回一个空的Constructor对象
						candidateConstructors = new Constructor<?>[0];
					}
					//将构造函数加入缓存
					// 12.将beanClass的构造函数解析结果放到缓存
					this.candidateConstructorsCache.put(beanClass, candidateConstructors);
				}
			}
		}
		//返回构造函数
		// 13.返回解析的构造函数
		return (candidateConstructors.length > 0 ? candidateConstructors : null);
	}

	/**
	 * autowirted 依赖注入实现
	 *
	 * @param pvs
	 * @param bean
	 * @param beanName
	 * @return
	 */
	@Override
	public PropertyValues postProcessProperties(PropertyValues pvs, Object bean, String beanName) {
		//1. 获取搜集到的class中需要注入的metadata，因为上一步已经搜集过了可以从缓存中直接拿到
		InjectionMetadata metadata = findAutowiringMetadata(beanName, bean.getClass(), pvs);
		try {
			// 2.InjectionMetadata: 执行inject()方法，开始执行属性注入或方法注入
			metadata.inject(bean, beanName, pvs);
		} catch (BeanCreationException ex) {
			throw ex;
		} catch (Throwable ex) {
			throw new BeanCreationException(beanName, "Injection of autowired dependencies failed", ex);
		}
		return pvs;
	}

	@Deprecated
	@Override
	public PropertyValues postProcessPropertyValues(
			PropertyValues pvs, PropertyDescriptor[] pds, Object bean, String beanName) {

		return postProcessProperties(pvs, bean, beanName);
	}

	/**
	 * 'Native' processing method for direct calls with an arbitrary target instance,
	 * resolving all of its fields and methods which are annotated with one of the
	 * configured 'autowired' annotation types.
	 *
	 * @param bean the target instance to process
	 * @throws BeanCreationException if autowiring failed
	 * @see #setAutowiredAnnotationTypes(Set)
	 */
	public void processInjection(Object bean) throws BeanCreationException {
		Class<?> clazz = bean.getClass();
		InjectionMetadata metadata = findAutowiringMetadata(clazz.getName(), clazz, null);
		try {
			metadata.inject(bean, null, null);
		} catch (BeanCreationException ex) {
			throw ex;
		} catch (Throwable ex) {
			throw new BeanCreationException(
					"Injection of autowired dependencies failed for class [" + clazz + "]", ex);
		}
	}

	/**
	 * 搜集方法以及字段的@Autowired注解并封装成InjectionMetadata
	 *
	 * @param beanName
	 * @param clazz
	 * @param pvs
	 * @return
	 */
	private InjectionMetadata findAutowiringMetadata(String beanName, Class<?> clazz, @Nullable PropertyValues pvs) {
		// Fall back to class name as cache key, for backwards compatibility with custom callers.
		// 1.设置cacheKey的值（beanName 或者 className）
		String cacheKey = (StringUtils.hasLength(beanName) ? beanName : clazz.getName());
		// Quick check on the concurrent map first, with minimal locking.
		// 2.检查beanName对应的InjectionMetadata是否已经存在于缓存中
		//从缓存给中获取metadata
		//因为已经上一步已经搜集过了可以直接从缓存中拿到
		InjectionMetadata metadata = this.injectionMetadataCache.get(cacheKey);
		// 3.检查InjectionMetadata是否需要刷新（为空或者class变了）
		//检查是否刷新如果metadata为null或者class 不匹配就刷新
		if (InjectionMetadata.needsRefresh(metadata, clazz)) {
			synchronized (this.injectionMetadataCache) {
				// 4.加锁后，再次从缓存中获取beanName对应的InjectionMetadata
				//从缓存给中获取metadata
				metadata = this.injectionMetadataCache.get(cacheKey);
				// 5.加锁后，再次检查InjectionMetadata是否需要刷新
				//双重检查锁
				//检查是否刷新如果metadata为null或者class 不匹配就刷新
				if (InjectionMetadata.needsRefresh(metadata, clazz)) {
					if (metadata != null) {
						// 6.如果需要刷新，并且metadata不为空，则先移除
						metadata.clear(pvs);
					}
					// 7.解析@Autowired注解的信息，生成元数据（包含clazz和clazz里解析到的注入的元素，
					// 这里的元素包括AutowiredFieldElement和AutowiredMethodElement）
					metadata = buildAutowiringMetadata(clazz);
					// 8.将解析的元数据放到injectionMetadataCache缓存，以备复用，每一个类只解析一次
					this.injectionMetadataCache.put(cacheKey, metadata);
				}
			}
		}
		return metadata;
	}

	/**
	 * 搜集方法以及字段的@Autowired注解并封装成InjectionMetadata
	 *
	 * @param clazz
	 * @return
	 */
	private InjectionMetadata buildAutowiringMetadata(final Class<?> clazz) {
		//1. 检查clazz中是否包含@Autowired以及@Value注解
		if (!AnnotationUtils.isCandidateClass(clazz, this.autowiredAnnotationTypes)) {
			//1.1 如果不包含直接返回
			return InjectionMetadata.EMPTY;
		}

		// 2.用于存放所有解析到的注入的元素的变量
		List<InjectionMetadata.InjectedElement> elements = new ArrayList<>();
		Class<?> targetClass = clazz;
		// 3.循环遍历
		do {
			// 3.1 定义存放当前循环的Class注入的元素(有序)
			final List<InjectionMetadata.InjectedElement> currElements = new ArrayList<>();
			// 3.2 如果targetClass的属性上有@Autowired注解，则用工具类获取注解信息
			ReflectionUtils.doWithLocalFields(targetClass, field -> {
				// 3.2.1 获取field上的@Autowired注解信息
				//找到有@Autowired注解的字段
				MergedAnnotation<?> ann = findAutowiredAnnotation(field);
				if (ann != null) {
					// 3.2.2 校验field是否被static修饰，如果是则直接返回，因为@Autowired注解不支持static修饰的field
					if (Modifier.isStatic(field.getModifiers())) {
						if (logger.isInfoEnabled()) {
							logger.info("Autowired annotation is not supported on static fields: " + field);
						}
						return;
					}
					// 3.2.3 获取@Autowired注解的required的属性值（required：值为true时，如果没有找到bean时，自动装配应该失败；false则不会）
					boolean required = determineRequiredStatus(ann);
					//生成AutowiredFieldElement对象并加入到currElements
					// 3.2.4 将field、required封装成AutowiredFieldElement，添加到currElements
					currElements.add(new AutowiredFieldElement(field, required));
				}
			});
			// 3.3 如果targetClass的方法上有@Autowired注解，则用工具类获取注解信息
			ReflectionUtils.doWithLocalMethods(targetClass, method -> {
				//3.3.1. 找出我们在代码中定义的方法而非编译器为我们生成的方法
				Method bridgedMethod = BridgeMethodResolver.findBridgedMethod(method);
				// 3.3.2 判断方法的可见性，如果不可见则直接返回
				if (!BridgeMethodResolver.isVisibilityBridgeMethodPair(method, bridgedMethod)) {
					return;
				}
				//根据@Autowired注解查找对应的方法
				MergedAnnotation<?> ann = findAutowiredAnnotation(bridgedMethod);
				//3.3.3. 如果重写了父类的方法，则使用子类的
				if (ann != null && method.equals(ClassUtils.getMostSpecificMethod(method, clazz))) {
					// 3.3.4 校验method是否被static修饰，如果是则直接返回，因为@Autowired注解不支持static修饰的method
					if (Modifier.isStatic(method.getModifiers())) {
						if (logger.isInfoEnabled()) {
							logger.info("Autowired annotation is not supported on static methods: " + method);
						}
						return;
					}
					// 3.3.5 @Autowired注解标识在方法上的目的就是将容器内的Bean注入到方法的参数中，没有参数就违背了初衷
					if (method.getParameterCount() == 0) {
						if (logger.isInfoEnabled()) {
							logger.info("Autowired annotation should only be used on methods with parameters: " +
									method);
						}
					}
					// 3.3.6 获取@Autowired注解的required的属性值
					boolean required = determineRequiredStatus(ann);
					// 3.3.7  获取method的属性描述器
					PropertyDescriptor pd = BeanUtils.findPropertyForMethod(bridgedMethod, clazz);
					//生成AutowiredFieldElement对象并加入到currElements
					// 3.3.8 将method、required、pd封装成AutowiredMethodElement，添加到currElements
					currElements.add(new AutowiredMethodElement(method, required, pd));
				}
			});
			// 3.4 将本次循环获取到的注解信息添加到elements
			//将currElements添加到elements
			elements.addAll(0, currElements);
			//将targetClass的父类赋值给targetClass
			// 3.5 在解析完targetClass之后，递归解析父类，将所有的@Autowired的属性和方法收集起来，且类的层级越高其属性会被越优先注入
			targetClass = targetClass.getSuperclass();
		}
		//3.6 如果targetClass不是Object继续循环
		while (targetClass != null && targetClass != Object.class);
		//将elements封装成InjectionMetadata
		// 3.7 将clazz和解析到的注入的元素封装成InjectionMetadata
		return InjectionMetadata.forElements(elements, clazz);
	}

	/**
	 * 查找Autowired的注解
	 *
	 * @param ao 属性 方法 构造函数的父类
	 * @return
	 */
	@Nullable
	private MergedAnnotation<?> findAutowiredAnnotation(AccessibleObject ao) {
		// 1.判断ao是否有被注解修饰
		MergedAnnotations annotations = MergedAnnotations.from(ao);
		// 2.检查是否有autowiredAnnotationTypes中的注解：@Autowired、@Value（@Value无法修饰构造函数）
		for (Class<? extends Annotation> type : this.autowiredAnnotationTypes) {
			// 3.拿到注解的合并注解属性，@Autowire在这边拿到，required=true（默认属性）
			MergedAnnotation<?> annotation = annotations.get(type);
			if (annotation.isPresent()) {
				return annotation;
			}
		}
		return null;
	}

	/**
	 * Determine if the annotated field or method requires its dependency.
	 * <p>A 'required' dependency means that autowiring should fail when no beans
	 * are found. Otherwise, the autowiring process will simply bypass the field
	 * or method when no beans are found.
	 *
	 * @param ann the Autowired annotation
	 * @return whether the annotation indicates that a dependency is required
	 */
	@SuppressWarnings({"deprecation", "cast"})
	protected boolean determineRequiredStatus(MergedAnnotation<?> ann) {
		// The following (AnnotationAttributes) cast is required on JDK 9+.
		return determineRequiredStatus((AnnotationAttributes)
				ann.asMap(mergedAnnotation -> new AnnotationAttributes(mergedAnnotation.getType())));
	}

	/**
	 * Determine if the annotated field or method requires its dependency.
	 * <p>A 'required' dependency means that autowiring should fail when no beans
	 * are found. Otherwise, the autowiring process will simply bypass the field
	 * or method when no beans are found.
	 *
	 * @param ann the Autowired annotation
	 * @return whether the annotation indicates that a dependency is required
	 * @deprecated since 5.2, in favor of {@link #determineRequiredStatus(MergedAnnotation)}
	 */
	@Deprecated
	protected boolean determineRequiredStatus(AnnotationAttributes ann) {
		return (!ann.containsKey(this.requiredParameterName) ||
				this.requiredParameterValue == ann.getBoolean(this.requiredParameterName));
	}

	/**
	 * Obtain all beans of the given type as autowire candidates.
	 *
	 * @param type the type of the bean
	 * @return the target beans, or an empty Collection if no bean of this type is found
	 * @throws BeansException if bean retrieval failed
	 */
	protected <T> Map<String, T> findAutowireCandidates(Class<T> type) throws BeansException {
		if (this.beanFactory == null) {
			throw new IllegalStateException("No BeanFactory configured - " +
					"override the getBeanOfType method or specify the 'beanFactory' property");
		}
		return BeanFactoryUtils.beansOfTypeIncludingAncestors(this.beanFactory, type);
	}

	/**
	 * Register the specified bean as dependent on the autowired beans.
	 * 注册依赖的bean
	 */
	private void registerDependentBeans(@Nullable String beanName, Set<String> autowiredBeanNames) {
		if (beanName != null) {
			// 1.遍历所有autowiredBeanNames
			for (String autowiredBeanName : autowiredBeanNames) {
				//如果beanFactory有这个依赖的bean
				if (this.beanFactory != null && this.beanFactory.containsBean(autowiredBeanName)) {
					// 2.如果autowiredBeanName在BeanFactory中存在，则注册依赖关系到缓存（beanName 依赖 autowiredBeanName）
					this.beanFactory.registerDependentBean(autowiredBeanName, beanName);
				}
				if (logger.isTraceEnabled()) {
					logger.trace("Autowiring by type from bean name '" + beanName +
							"' to bean named '" + autowiredBeanName + "'");
				}
			}
		}
	}

	/**
	 * Resolve the specified cached method argument or field value.
	 * 获取 缓存的参数
	 */
	@Nullable
	private Object resolvedCachedArgument(@Nullable String beanName, @Nullable Object cachedArgument) {
		if (cachedArgument instanceof DependencyDescriptor) {
			DependencyDescriptor descriptor = (DependencyDescriptor) cachedArgument;
			Assert.state(this.beanFactory != null, "No BeanFactory available");
			//获取依赖的对象
			return this.beanFactory.resolveDependency(descriptor, beanName, null, null);
		} else {
			return cachedArgument;
		}
	}


	/**
	 * Class representing injection information about an annotated field.
	 */
	private class AutowiredFieldElement extends InjectionMetadata.InjectedElement {

		private final boolean required;

		private volatile boolean cached = false;

		@Nullable
		private volatile Object cachedFieldValue;

		public AutowiredFieldElement(Field field, boolean required) {
			super(field, null);
			this.required = required;
		}

		/**
		 * 属性的依赖注入
		 *
		 * @param bean
		 * @param beanName
		 * @param pvs
		 * @throws Throwable
		 */
		@Override
		protected void inject(Object bean, @Nullable String beanName, @Nullable PropertyValues pvs) throws Throwable {
			// 1.拿到该元数据的属性值
			Field field = (Field) this.member;
			Object value;
			// 2.如果缓存中已经存在，则直接从缓存中解析属性
			if (this.cached) {
				//获取缓存中的参数
				value = resolvedCachedArgument(beanName, this.cachedFieldValue);
			} else {
				// 3.将field封装成DependencyDescriptor
				DependencyDescriptor desc = new DependencyDescriptor(field, this.required);
				//设置class
				desc.setContainingClass(bean.getClass());
				Set<String> autowiredBeanNames = new LinkedHashSet<>(1);
				Assert.state(beanFactory != null, "No BeanFactory available");
				//获取类型转换器
				TypeConverter typeConverter = beanFactory.getTypeConverter();
				try {
					// 4.解析当前属性所匹配的bean实例，并把解析到的bean实例的beanName存储在autowiredBeanNames中
					value = beanFactory.resolveDependency(desc, beanName, autowiredBeanNames, typeConverter);
				} catch (BeansException ex) {
					throw new UnsatisfiedDependencyException(null, beanName, new InjectionPoint(field), ex);
				}
				synchronized (this) {
					if (!this.cached) {
						// 5.value不为空或者required为true
						if (value != null || this.required) {
							// 6.如果属性依赖注入的bean不止一个（Array,Collection,Map），缓存cachedFieldValue放的是DependencyDescriptor
							this.cachedFieldValue = desc;
							// 7.注册依赖关系到缓存（beanName 依赖 autowiredBeanNames）
							registerDependentBeans(beanName, autowiredBeanNames);
							// 8.如果属性依赖注入的bean只有一个（正常都是一个）
							if (autowiredBeanNames.size() == 1) {
								String autowiredBeanName = autowiredBeanNames.iterator().next();
								// @Autowired标识属性类型和Bean的类型要匹配，因此Array,Collection,Map类型的属性不支持缓存属性Bean名称
								// 9.检查autowiredBeanName对应的bean的类型是否为field的类型
								if (beanFactory.containsBean(autowiredBeanName) &&
										beanFactory.isTypeMatch(autowiredBeanName, field.getType())) {
									// 10.将该属性解析到的bean的信息封装成ShortcutDependencyDescriptor，
									// 以便之后可以通过getBean方法来快速拿到bean实例
									this.cachedFieldValue = new ShortcutDependencyDescriptor(
											desc, autowiredBeanName, field.getType());
								}
							}
						} else {
							this.cachedFieldValue = null;
						}
						// 11.缓存标识设为true
						this.cached = true;
					}
				}
			}
			if (value != null) {
				// 12.设置字段访问性
				ReflectionUtils.makeAccessible(field);
				// 13.通过反射为属性赋值，将解析出来的bean实例赋值给field
				field.set(bean, value);
			}
		}
	}


	/**
	 * Class representing injection information about an annotated method.
	 */
	private class AutowiredMethodElement extends InjectionMetadata.InjectedElement {

		private final boolean required;

		private volatile boolean cached = false;

		@Nullable
		private volatile Object[] cachedMethodArguments;

		public AutowiredMethodElement(Method method, boolean required, @Nullable PropertyDescriptor pd) {
			super(method, pd);
			this.required = required;
		}

		/**
		 * 方法的依赖注入
		 *
		 * @param bean
		 * @param beanName
		 * @param pvs
		 * @throws Throwable
		 */
		@Override
		protected void inject(Object bean, @Nullable String beanName, @Nullable PropertyValues pvs) throws Throwable {
			if (checkPropertySkipping(pvs)) {
				return;
			}
			// 1.拿到该元数据的属性值
			Method method = (Method) this.member;
			Object[] arguments;
			// 2.如果缓存中已经存在，则直接从缓存中解析属性
			if (this.cached) {
				// Shortcut for avoiding synchronization...
				//2.1 获取对象方法(依赖对象)的参数
				arguments = resolveCachedArguments(beanName);
			} else {
				//3. 获取方法参数(依赖对象)
				int argumentCount = method.getParameterCount();
				arguments = new Object[argumentCount];
				//4. 根据参数(依赖对象)创建DependencyDescriptor数组
				DependencyDescriptor[] descriptors = new DependencyDescriptor[argumentCount];
				Set<String> autowiredBeans = new LinkedHashSet<>(argumentCount);
				Assert.state(beanFactory != null, "No BeanFactory available");
				//5. 获取参数(依赖对象)类型转换器
				TypeConverter typeConverter = beanFactory.getTypeConverter();
				//6. 遍历所有参数
				for (int i = 0; i < arguments.length; i++) {
					//6.1 创建方法参数(依赖对象)的封装
					MethodParameter methodParam = new MethodParameter(method, i);
					//6.2 将方法参数(依赖对象)封装成DependencyDescriptor
					DependencyDescriptor currDesc = new DependencyDescriptor(methodParam, this.required);
					//设置BenaClass
					currDesc.setContainingClass(bean.getClass());
					//进行数组赋值
					descriptors[i] = currDesc;
					try {
						// 6.3 解析当前属性所匹配的bean(依赖对象)实例，并把解析到的bean实例的beanName存储在autowiredBeanNames中
						Object arg = beanFactory.resolveDependency(currDesc, beanName, autowiredBeans, typeConverter);
						//6.4 如果获取到的参数(依赖对象)为空 && 并且是非required的
						if (arg == null && !this.required) {
							// 将 参数设置为空
							arguments = null;
							//并跳出循环
							break;
						}
						//6.5 否则设置数组参数(依赖对象)
						arguments[i] = arg;
					} catch (BeansException ex) {
						throw new UnsatisfiedDependencyException(null, beanName, new InjectionPoint(methodParam), ex);
					}
				}
				synchronized (this) {
					if (!this.cached) {
						if (arguments != null) {
							//7. 对依赖对象进行数组拷贝
							DependencyDescriptor[] cachedMethodArguments = Arrays.copyOf(descriptors, arguments.length);
							//8. 注册依赖关系到缓存（beanName 依赖 autowiredBeanNames）
							registerDependentBeans(beanName, autowiredBeans);
							if (autowiredBeans.size() == argumentCount) {
								//9. 创建autowiredBeans的迭代器
								Iterator<String> it = autowiredBeans.iterator();
								Class<?>[] paramTypes = method.getParameterTypes();
								for (int i = 0; i < paramTypes.length; i++) {
									String autowiredBeanName = it.next();
									// @Autowired标识属性类型和Bean的类型要匹配，因此Array,Collection,Map类型的属性不支持缓存属性Bean名称
									// 10.检查autowiredBeanName对应的bean的类型是否为field的类型
									if (beanFactory.containsBean(autowiredBeanName) &&
											beanFactory.isTypeMatch(autowiredBeanName, paramTypes[i])) {
										// 11.将该属性解析到的bean的信息封装成ShortcutDependencyDescriptor，
										// 以便之后可以通过getBean方法来快速拿到bean实例
										cachedMethodArguments[i] = new ShortcutDependencyDescriptor(
												descriptors[i], autowiredBeanName, paramTypes[i]);
									}
								}
							}
							this.cachedMethodArguments = cachedMethodArguments;
						} else {
							this.cachedMethodArguments = null;
						}
						// 12.缓存标识设为true
						this.cached = true;
					}
				}
			}
			if (arguments != null) {
				try {
					// 13.设置字段访问性
					ReflectionUtils.makeAccessible(method);
					//14. 反射调用方法注入
					method.invoke(bean, arguments);
				} catch (InvocationTargetException ex) {
					throw ex.getTargetException();
				}
			}
		}

		@Nullable
		private Object[] resolveCachedArguments(@Nullable String beanName) {
			Object[] cachedMethodArguments = this.cachedMethodArguments;
			if (cachedMethodArguments == null) {
				return null;
			}
			Object[] arguments = new Object[cachedMethodArguments.length];
			for (int i = 0; i < arguments.length; i++) {
				arguments[i] = resolvedCachedArgument(beanName, cachedMethodArguments[i]);
			}
			return arguments;
		}
	}


	/**
	 * DependencyDescriptor variant with a pre-resolved target bean name.
	 */
	@SuppressWarnings("serial")
	private static class ShortcutDependencyDescriptor extends DependencyDescriptor {

		private final String shortcut;

		private final Class<?> requiredType;

		public ShortcutDependencyDescriptor(DependencyDescriptor original, String shortcut, Class<?> requiredType) {
			super(original);
			this.shortcut = shortcut;
			this.requiredType = requiredType;
		}

		@Override
		public Object resolveShortcut(BeanFactory beanFactory) {
			return beanFactory.getBean(this.shortcut, this.requiredType);
		}
	}

}
