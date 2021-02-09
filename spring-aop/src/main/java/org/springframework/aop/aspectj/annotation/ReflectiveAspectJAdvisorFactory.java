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

package org.springframework.aop.aspectj.annotation;

import java.io.Serializable;
import java.lang.annotation.Annotation;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.util.ArrayList;
import java.util.Comparator;
import java.util.List;

import org.aopalliance.aop.Advice;
import org.aspectj.lang.annotation.After;
import org.aspectj.lang.annotation.AfterReturning;
import org.aspectj.lang.annotation.AfterThrowing;
import org.aspectj.lang.annotation.Around;
import org.aspectj.lang.annotation.Before;
import org.aspectj.lang.annotation.DeclareParents;
import org.aspectj.lang.annotation.Pointcut;

import org.springframework.aop.Advisor;
import org.springframework.aop.MethodBeforeAdvice;
import org.springframework.aop.aspectj.AbstractAspectJAdvice;
import org.springframework.aop.aspectj.AspectJAfterAdvice;
import org.springframework.aop.aspectj.AspectJAfterReturningAdvice;
import org.springframework.aop.aspectj.AspectJAfterThrowingAdvice;
import org.springframework.aop.aspectj.AspectJAroundAdvice;
import org.springframework.aop.aspectj.AspectJExpressionPointcut;
import org.springframework.aop.aspectj.AspectJMethodBeforeAdvice;
import org.springframework.aop.aspectj.DeclareParentsAdvisor;
import org.springframework.aop.framework.AopConfigException;
import org.springframework.aop.support.DefaultPointcutAdvisor;
import org.springframework.beans.factory.BeanFactory;
import org.springframework.core.annotation.AnnotationUtils;
import org.springframework.core.convert.converter.Converter;
import org.springframework.core.convert.converter.ConvertingComparator;
import org.springframework.lang.Nullable;
import org.springframework.util.ReflectionUtils;
import org.springframework.util.StringUtils;
import org.springframework.util.comparator.InstanceComparator;

/**
 * Factory that can create Spring AOP Advisors given AspectJ classes from
 * classes honoring the AspectJ 5 annotation syntax, using reflection to
 * invoke the corresponding advice methods.
 *
 * @author Rod Johnson
 * @author Adrian Colyer
 * @author Juergen Hoeller
 * @author Ramnivas Laddad
 * @author Phillip Webb
 * @since 2.0
 */
@SuppressWarnings("serial")
public class ReflectiveAspectJAdvisorFactory extends AbstractAspectJAdvisorFactory implements Serializable {

	private static final Comparator<Method> METHOD_COMPARATOR;

	static {
		Comparator<Method> adviceKindComparator = new ConvertingComparator<>(
				new InstanceComparator<>(
						Around.class, Before.class, After.class, AfterReturning.class, AfterThrowing.class),
				(Converter<Method, Annotation>) method -> {
					AspectJAnnotation<?> annotation =
						AbstractAspectJAdvisorFactory.findAspectJAnnotationOnMethod(method);
					return (annotation != null ? annotation.getAnnotation() : null);
				});
		Comparator<Method> methodNameComparator = new ConvertingComparator<>(Method::getName);
		METHOD_COMPARATOR = adviceKindComparator.thenComparing(methodNameComparator);
	}


	@Nullable
	private final BeanFactory beanFactory;


	/**
	 * Create a new {@code ReflectiveAspectJAdvisorFactory}.
	 */
	public ReflectiveAspectJAdvisorFactory() {
		this(null);
	}

	/**
	 * Create a new {@code ReflectiveAspectJAdvisorFactory}, propagating the given
	 * {@link BeanFactory} to the created {@link AspectJExpressionPointcut} instances,
	 * for bean pointcut handling as well as consistent {@link ClassLoader} resolution.
	 * @param beanFactory the BeanFactory to propagate (may be {@code null}}
	 * @since 4.3.6
	 * @see AspectJExpressionPointcut#setBeanFactory
	 * @see org.springframework.beans.factory.config.ConfigurableBeanFactory#getBeanClassLoader()
	 */
	public ReflectiveAspectJAdvisorFactory(@Nullable BeanFactory beanFactory) {
		this.beanFactory = beanFactory;
	}


	/**
	 *   解析标记AspectJ注解中的增强方法
	 * @param aspectInstanceFactory the aspect instance factory
	 * (not the aspect instance itself in order to avoid eager instantiation)
	 * @return
	 */
	@Override
	public List<Advisor> getAdvisors(MetadataAwareAspectInstanceFactory aspectInstanceFactory) {
		// 1.从工厂中获取有@Aspect注解的类Class
		// 前面我们将beanClass和beanName封装成了aspectInstanceFactory的AspectMetadata属性，
		// 这边可以通过AspectMetadata属性重新获取到当前处理的切面类
		Class<?> aspectClass = aspectInstanceFactory.getAspectMetadata().getAspectClass();
		// 2.获取当前处理的切面类的名字
		//从工厂中获取有@Aspect注解的类的名称
		String aspectName = aspectInstanceFactory.getAspectMetadata().getAspectName();
		// 3.校验切面类
		validate(aspectClass);

		// We need to wrap the MetadataAwareAspectInstanceFactory with a decorator
		// so that it will only instantiate once.
		// 4.使用装饰器包装MetadataAwareAspectInstanceFactory，以便它只实例化一次。
		//创建工厂的装饰类，获取实例只会获取一次
		MetadataAwareAspectInstanceFactory lazySingletonAspectInstanceFactory =
				new LazySingletonAspectInstanceFactoryDecorator(aspectInstanceFactory);

		// 5.获取切面类中的方法（也就是我们用来进行逻辑增强的方法，被@Around、@After等注解修饰的方法，使用@Pointcut的方法不处理）
		//这里循环没有@Pointcut注解的方法
		List<Advisor> advisors = new ArrayList<>();
		for (Method method : getAdvisorMethods(aspectClass)) {
			// 6.处理method，获取增强器
			//非常重要重点看看，重要程度 5
			Advisor advisor = getAdvisor(method, lazySingletonAspectInstanceFactory, advisors.size(), aspectName);
			if (advisor != null) {
				// 7.如果增强器不为空，则添加到advisors
				advisors.add(advisor);
			}
		}

		// If it's a per target aspect, emit the dummy instantiating aspect.
		// 8.如果寻找的增强器不为空而且又配置了增强延迟初始化，那么需要在首位加入同步实例化增强器（用以保证增强使用之前的实例化）
		if (!advisors.isEmpty() && lazySingletonAspectInstanceFactory.getAspectMetadata().isLazilyInstantiated()) {
			Advisor instantiationAdvisor = new SyntheticInstantiationAdvisor(lazySingletonAspectInstanceFactory);
			advisors.add(0, instantiationAdvisor);
		}

		// 9.获取DeclareParents注解
		//判断属性上是否有引介注解
		// Find introduction fields.
		for (Field field : aspectClass.getDeclaredFields()) {
			//判断属性上是否有DeclareParents注解，如果有返回切面
			Advisor advisor = getDeclareParentsAdvisor(field);
			if (advisor != null) {
				advisors.add(advisor);
			}
		}

		return advisors;
	}

	private List<Method> getAdvisorMethods(Class<?> aspectClass) {
		final List<Method> methods = new ArrayList<>();
		ReflectionUtils.doWithMethods(aspectClass, method -> {
			// Exclude pointcuts
			if (AnnotationUtils.getAnnotation(method, Pointcut.class) == null) {
				methods.add(method);
			}
		}, ReflectionUtils.USER_DECLARED_METHODS);
		methods.sort(METHOD_COMPARATOR);
		return methods;
	}

	/**
	 * Build a {@link org.springframework.aop.aspectj.DeclareParentsAdvisor}
	 * for the given introduction field.
	 * <p>Resulting Advisors will need to be evaluated for targets.
	 * @param introductionField the field to introspect
	 * @return the Advisor instance, or {@code null} if not an Advisor
	 */
	@Nullable
	private Advisor getDeclareParentsAdvisor(Field introductionField) {
		DeclareParents declareParents = introductionField.getAnnotation(DeclareParents.class);
		if (declareParents == null) {
			// Not an introduction field
			return null;
		}

		if (DeclareParents.class == declareParents.defaultImpl()) {
			throw new IllegalStateException("'defaultImpl' attribute must be set on DeclareParents");
		}

		return new DeclareParentsAdvisor(
				introductionField.getType(), declareParents.value(), declareParents.defaultImpl());
	}


	/**
	 * 处理method，获取增强器
	 * @return
	 */
	@Override
	@Nullable
	public Advisor getAdvisor(Method candidateAdviceMethod, MetadataAwareAspectInstanceFactory aspectInstanceFactory,
			int declarationOrderInAspect, String aspectName) {
		// 1.校验切面类
		validate(aspectInstanceFactory.getAspectMetadata().getAspectClass());
		// 2.AspectJ切点信息的获取（例如：表达式），就是指定注解的表达式信息的获取，如：@Around("execution(* com.joonwhee.open.aop.*.*(..))")
		//获取pointCut对象，最重要的是从注解中获取表达式
		AspectJExpressionPointcut expressionPointcut = getPointcut(
				candidateAdviceMethod, aspectInstanceFactory.getAspectMetadata().getAspectClass());
		// 3.如果expressionPointcut为null，则直接返回null

		if (expressionPointcut == null) {
			return null;
		}
		//重要程度 5
		// 4.根据切点信息生成增强器
		//创建Advisor切面类，这才是真正的切面类，一个切面类里面肯定要有1、pointCut 2、advice
		//这里pointCut是expressionPointcut， advice 增强方法是 candidateAdviceMethod
		return new InstantiationModelAwarePointcutAdvisorImpl(expressionPointcut, candidateAdviceMethod,
				this, aspectInstanceFactory, declarationOrderInAspect, aspectName);
	}

	/**
	 * 获取切点表达式
	 *
	 * @param candidateAdviceMethod
	 * @param candidateAspectClass
	 * @return
	 */
	@Nullable
	private AspectJExpressionPointcut getPointcut(Method candidateAdviceMethod, Class<?> candidateAspectClass) {
		// 1.查找并返回给定方法的第一个AspectJ注解（@Before, @Around, @After, @AfterReturning, @AfterThrowing, @Pointcut）
		// 因为我们之前把@Pointcut注解的方法跳过了，所以这边必然不会获取到@Pointcut注解
		//从候选的增强方法里面 candidateAdviceMethod  找有有注解
		//Pointcut.class, Around.class, Before.class, After.class, AfterReturning.class, AfterThrowing.class
		//并把注解信息封装成AspectJAnnotation对象
		AspectJAnnotation<?> aspectJAnnotation =
				AbstractAspectJAdvisorFactory.findAspectJAnnotationOnMethod(candidateAdviceMethod);
		// 2.如果方法没有使用AspectJ的注解，则返回null

		if (aspectJAnnotation == null) {
			return null;
		}
		// 3.使用AspectJExpressionPointcut实例封装获取的信息
		//创建一个PointCut类，并且把前面从注解里面解析的表达式设置进去
		AspectJExpressionPointcut ajexp =
				new AspectJExpressionPointcut(candidateAspectClass, new String[0], new Class<?>[0]);
		//将刚刚解析注解的pointCut设置进ajexp
		// 提取得到的注解中的表达式，
		// 例如：@Around("execution(* com.test.spring.aop.*.*(..))")，得到：execution(* com.test.spring.aop.*.*(..))

		ajexp.setExpression(aspectJAnnotation.getPointcutExpression());
		if (this.beanFactory != null) {
			ajexp.setBeanFactory(this.beanFactory);
		}
		//返回Pointcut
		return ajexp;
	}


	/**
	 * 获取增强
	 *
	 * @return
	 */
	@Override
	@Nullable
	public Advice getAdvice(Method candidateAdviceMethod, AspectJExpressionPointcut expressionPointcut,
			MetadataAwareAspectInstanceFactory aspectInstanceFactory, int declarationOrder, String aspectName) {
		// 1.获取切面类
		//获取有@Aspect注解的类
		Class<?> candidateAspectClass = aspectInstanceFactory.getAspectMetadata().getAspectClass();
		// 2.校验切面类（重复校验第3次...）

		validate(candidateAspectClass);
		// 3.查找并返回方法的第一个AspectJ注解
		//找到candidateAdviceMethod方法上面的注解，并且包装成AspectJAnnotation对象，这个对象中就有注解类型
		AspectJAnnotation<?> aspectJAnnotation =
				AbstractAspectJAdvisorFactory.findAspectJAnnotationOnMethod(candidateAdviceMethod);
		if (aspectJAnnotation == null) {
			return null;
		}

		// If we get here, we know we have an AspectJ method.
		// Check that it's an AspectJ-annotated class
		// 4.如果我们到这里，我们知道我们有一个AspectJ方法。检查切面类是否使用了AspectJ注解

		if (!isAspect(candidateAspectClass)) {
			throw new AopConfigException("Advice must be declared inside an aspect type: " +
					"Offending method '" + candidateAdviceMethod + "' in class [" +
					candidateAspectClass.getName() + "]");
		}

		if (logger.isDebugEnabled()) {
			logger.debug("Found AspectJ method: " + candidateAdviceMethod);
		}

		AbstractAspectJAdvice springAdvice;
		// 5.根据方法使用的aspectJ注解创建对应的增强器，例如最常见的@Around注解会创建AspectJAroundAdvice
		//根据不同的注解类型创建不同的advice类实例
		switch (aspectJAnnotation.getAnnotationType()) {
			case AtPointcut:
				if (logger.isDebugEnabled()) {
					logger.debug("Processing pointcut '" + candidateAdviceMethod.getName() + "'");
				}
				return null;
			case AtAround:
				//实现了MethodInterceptor接口

				springAdvice = new AspectJAroundAdvice(
						candidateAdviceMethod, expressionPointcut, aspectInstanceFactory);
				break;
			case AtBefore:
				//实现了MethodBeforeAdvice接口，没有实现MethodInterceptor接口

				springAdvice = new AspectJMethodBeforeAdvice(
						candidateAdviceMethod, expressionPointcut, aspectInstanceFactory);
				break;
			case AtAfter:
				//实现了MethodInterceptor接口

				springAdvice = new AspectJAfterAdvice(
						candidateAdviceMethod, expressionPointcut, aspectInstanceFactory);
				break;
			case AtAfterReturning:
				//实现了AfterReturningAdvice接口，没有实现MethodInterceptor接口

				springAdvice = new AspectJAfterReturningAdvice(
						candidateAdviceMethod, expressionPointcut, aspectInstanceFactory);
				AfterReturning afterReturningAnnotation = (AfterReturning) aspectJAnnotation.getAnnotation();
				if (StringUtils.hasText(afterReturningAnnotation.returning())) {
					springAdvice.setReturningName(afterReturningAnnotation.returning());
				}
				break;
			case AtAfterThrowing:
				//实现了MethodInterceptor接口

				springAdvice = new AspectJAfterThrowingAdvice(
						candidateAdviceMethod, expressionPointcut, aspectInstanceFactory);
				AfterThrowing afterThrowingAnnotation = (AfterThrowing) aspectJAnnotation.getAnnotation();
				if (StringUtils.hasText(afterThrowingAnnotation.throwing())) {
					springAdvice.setThrowingName(afterThrowingAnnotation.throwing());
				}
				break;
			default:
				throw new UnsupportedOperationException(
						"Unsupported advice type on method: " + candidateAdviceMethod);
		}

		// Now to configure the advice...
		// 6.配置增强器
		// 切面类的name，其实就是beanName
		springAdvice.setAspectName(aspectName);
		springAdvice.setDeclarationOrder(declarationOrder);
		// 获取增强方法的参数

		String[] argNames = this.parameterNameDiscoverer.getParameterNames(candidateAdviceMethod);
		if (argNames != null) {
			// 如果参数不为空，则赋值给springAdvice

			springAdvice.setArgumentNamesFromStringArray(argNames);
		}
		//计算argNames和类型的对应关系

		springAdvice.calculateArgumentBindings();
		// 最后，返回增强器

		return springAdvice;
	}


	/**
	 * Synthetic advisor that instantiates the aspect.
	 * Triggered by per-clause pointcut on non-singleton aspect.
	 * The advice has no effect.
	 */
	@SuppressWarnings("serial")
	protected static class SyntheticInstantiationAdvisor extends DefaultPointcutAdvisor {

		public SyntheticInstantiationAdvisor(final MetadataAwareAspectInstanceFactory aif) {
			super(aif.getAspectMetadata().getPerClausePointcut(), (MethodBeforeAdvice)
					(method, args, target) -> aif.getAspectInstance());
		}
	}

}
