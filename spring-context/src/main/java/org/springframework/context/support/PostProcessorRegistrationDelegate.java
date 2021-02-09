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

package org.springframework.context.support;

import java.util.ArrayList;
import java.util.Collection;
import java.util.Comparator;
import java.util.HashSet;
import java.util.List;
import java.util.Set;

import org.apache.commons.logging.Log;
import org.apache.commons.logging.LogFactory;

import org.springframework.beans.factory.config.BeanDefinition;
import org.springframework.beans.factory.config.BeanFactoryPostProcessor;
import org.springframework.beans.factory.config.BeanPostProcessor;
import org.springframework.beans.factory.config.ConfigurableListableBeanFactory;
import org.springframework.beans.factory.support.BeanDefinitionRegistry;
import org.springframework.beans.factory.support.BeanDefinitionRegistryPostProcessor;
import org.springframework.beans.factory.support.DefaultListableBeanFactory;
import org.springframework.beans.factory.support.MergedBeanDefinitionPostProcessor;
import org.springframework.beans.factory.support.RootBeanDefinition;
import org.springframework.core.OrderComparator;
import org.springframework.core.Ordered;
import org.springframework.core.PriorityOrdered;
import org.springframework.lang.Nullable;

/**
 * Delegate for AbstractApplicationContext's post-processor handling.
 *
 * @author Juergen Hoeller
 * @since 4.0
 */
final class PostProcessorRegistrationDelegate {

	private PostProcessorRegistrationDelegate() {
	}

	/**
	 * 主要的调用PostProcessor接口的入口进行实例化之前的操作
	 *
	 * @param beanFactory
	 * @param beanFactoryPostProcessors
	 */
	public static void invokeBeanFactoryPostProcessors(
			ConfigurableListableBeanFactory beanFactory, List<BeanFactoryPostProcessor> beanFactoryPostProcessors) {


		//优先调用BeanDefinitionRegistryPostProcessors进行注册
		// Invoke BeanDefinitionRegistryPostProcessors first, if any.
		Set<String> processedBeans = new HashSet<>();
		// 1.判断beanFactory是否为BeanDefinitionRegistry，beanFactory为DefaultListableBeanFactory,
		// 而DefaultListableBeanFactory实现了BeanDefinitionRegistry接口，因此这边为true
		if (beanFactory instanceof BeanDefinitionRegistry) {
			//将beanFactory转换为BeanDefinitionRegistry
			BeanDefinitionRegistry registry = (BeanDefinitionRegistry) beanFactory;
			// 用于存放BeanDefinitionRegistryPostProcessor
			List<BeanFactoryPostProcessor> regularPostProcessors = new ArrayList<>();
			// 用于存放普通的BeanFactoryPostProcessor
			List<BeanDefinitionRegistryPostProcessor> registryProcessors = new ArrayList<>();

			// 2.首先处理入参中的beanFactoryPostProcessors
			// 遍历所有的beanFactoryPostProcessors, 将BeanDefinitionRegistryPostProcessor和普通BeanFactoryPostProcessor区分开
			for (BeanFactoryPostProcessor postProcessor : beanFactoryPostProcessors) {
				// 2.1 如果实现了BeanDefinitionRegistryPostProcessor接口
				if (postProcessor instanceof BeanDefinitionRegistryPostProcessor) {
					//转换为BeanDefinitionRegistryPostProcessor类型
					BeanDefinitionRegistryPostProcessor registryProcessor =
							(BeanDefinitionRegistryPostProcessor) postProcessor;
					// 2.1.1 直接执行BeanDefinitionRegistryPostProcessor接口的postProcessBeanDefinitionRegistry方法
					registryProcessor.postProcessBeanDefinitionRegistry(registry);
					// 2.1.2 添加到registryProcessors(用于最后执行postProcessBeanFactory方法)
					registryProcessors.add(registryProcessor);
				} else {
					// 2.2 否则，只是普通的BeanFactoryPostProcessor
					// 2.2.1 添加到regularPostProcessors(用于最后执行postProcessBeanFactory方法)
					regularPostProcessors.add(postProcessor);
				}
			}

			// Do not initialize FactoryBeans here: We need to leave all regular beans
			// uninitialized to let the bean factory post-processors apply to them!
			// Separate between BeanDefinitionRegistryPostProcessors that implement
			// PriorityOrdered, Ordered, and the rest.
			// 用于保存本次要执行的BeanDefinitionRegistryPostProcessor
			List<BeanDefinitionRegistryPostProcessor> currentRegistryProcessors = new ArrayList<>();

			// 3.调用所有实现PriorityOrdered接口的BeanDefinitionRegistryPostProcessor实现类
			// 3.1 找出所有实现BeanDefinitionRegistryPostProcessor接口的Bean的beanName
			// First, invoke the BeanDefinitionRegistryPostProcessors that implement PriorityOrdered.
			String[] postProcessorNames =
					beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);

			// 3.2 遍历postProcessorNames
			for (String ppName : postProcessorNames) {
				// 3.3 校验是否实现了PriorityOrdered接口
				if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
					// 3.4 获取ppName对应的bean实例, 添加到currentRegistryProcessors中,
					// beanFactory.getBean: 这边getBean方法会触发创建ppName对应的bean对象, 目前暂不深入解析
					//因为需要进行调用所以先获取BeanDefinitionRegistryPostProcessor类型的实例并添加到currentRegistryProcessors
					currentRegistryProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
					// 3.5 将要被执行的加入processedBeans，避免后续重复执行
					processedBeans.add(ppName);
				}
			}
			// 3.6 进行排序(根据是否实现PriorityOrdered、Ordered接口和order值来排序)
			sortPostProcessors(currentRegistryProcessors, beanFactory);
			// 3.7 添加到registryProcessors(用于最后执行postProcessBeanFactory方法)
			registryProcessors.addAll(currentRegistryProcessors);

			// 3.8 遍历currentRegistryProcessors, 执行postProcessBeanDefinitionRegistry方法
			invokeBeanDefinitionRegistryPostProcessors(currentRegistryProcessors, registry);
			// 3.9 执行完毕后, 清空currentRegistryProcessors
			currentRegistryProcessors.clear();

			// 4.调用所有实现了Ordered接口的BeanDefinitionRegistryPostProcessor实现类（过程跟上面的步骤3基本一样）
			// 4.1 找出所有实现BeanDefinitionRegistryPostProcessor接口的类, 这边重复查找是因为执行完上面的BeanDefinitionRegistryPostProcessor,
			// 可能会新增了其他的BeanDefinitionRegistryPostProcessor, 因此需要重新查找
			// Next, invoke the BeanDefinitionRegistryPostProcessors that implement Ordered.
			postProcessorNames = beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);
			for (String ppName : postProcessorNames) {
				// 校验是否实现了Ordered接口，并且还未执行过
				if (!processedBeans.contains(ppName) && beanFactory.isTypeMatch(ppName, Ordered.class)) {
					//因为需要进行调用所以先获取BeanDefinitionRegistryPostProcessor类型的实例并添加到currentRegistryProcessors
					currentRegistryProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
					//将BeanDefinition对象的beanName添加到processedBeans
					processedBeans.add(ppName);
				}
			}
			//完成排序工作
			sortPostProcessors(currentRegistryProcessors, beanFactory);
			//添加到registryProcessors
			registryProcessors.addAll(currentRegistryProcessors);
			// 4.2 遍历currentRegistryProcessors, 执行postProcessBeanDefinitionRegistry方法
			invokeBeanDefinitionRegistryPostProcessors(currentRegistryProcessors, registry);
			//再次清空currentRegistryProcessors
			currentRegistryProcessors.clear();

			// 5.最后, 调用所有剩下的BeanDefinitionRegistryPostProcessors
			//没实现排序接口的调用
			// Finally, invoke all other BeanDefinitionRegistryPostProcessors until no further ones appear.
			boolean reiterate = true;
			while (reiterate) {
				//如果所以的BeanDefinitionRegistryPostProcessor都调用过了就退出循环
				reiterate = false;
				//5.1 获取实现了BeanDefinitionRegistryPostProcessor接口的所有类的BeanDefinition对象的beanName
				postProcessorNames = beanFactory.getBeanNamesForType(BeanDefinitionRegistryPostProcessor.class, true, false);
				//遍历postProcessorNames
				for (String ppName : postProcessorNames) {
					// 5.2 跳过已经执行过的
					//如果以上两次都没有调用过的BeanDefinitionRegistryPostProcessor走下面的判断
					if (!processedBeans.contains(ppName)) {
						//因为需要进行调用所以先获取BeanDefinitionRegistryPostProcessor类型的实例并添加到currentRegistryProcessors
						currentRegistryProcessors.add(beanFactory.getBean(ppName, BeanDefinitionRegistryPostProcessor.class));
						//将BeanDefinition对象的beanName添加到processedBeans
						processedBeans.add(ppName);
						//如果有调用的再次循环
						// 5.3 如果有BeanDefinitionRegistryPostProcessor被执行, 则有可能会产生新的BeanDefinitionRegistryPostProcessor,
						// 因此这边将reiterate赋值为true, 代表需要再循环查找一次
						reiterate = true;
					}
				}
				//完成排序工作
				sortPostProcessors(currentRegistryProcessors, beanFactory);
				//添加到registryProcessors
				registryProcessors.addAll(currentRegistryProcessors);
				// 5.4 遍历currentRegistryProcessors, 执行postProcessBeanDefinitionRegistry方法
				invokeBeanDefinitionRegistryPostProcessors(currentRegistryProcessors, registry);
				//再次清空currentRegistryProcessors
				currentRegistryProcessors.clear();
			}

			//如果registryProcessors不为空的话最后一次完成一次调用
			// 6.调用所有BeanDefinitionRegistryPostProcessor的postProcessBeanFactory方法(BeanDefinitionRegistryPostProcessor继承自BeanFactoryPostProcessor)
			// Now, invoke the postProcessBeanFactory callback of all processors handled so far.
			invokeBeanFactoryPostProcessors(registryProcessors, beanFactory);
			// 7.最后, 调用入参beanFactoryPostProcessors中的普通BeanFactoryPostProcessor的postProcessBeanFactory方法
			invokeBeanFactoryPostProcessors(regularPostProcessors, beanFactory);
		} else {
			// Invoke factory processors registered with the context instance.
			//完成BeanFactoryPostProcessor的调用
			invokeBeanFactoryPostProcessors(beanFactoryPostProcessors, beanFactory);
		}

		// 到这里 , 入参beanFactoryPostProcessors和容器中的所有BeanDefinitionRegistryPostProcessor已经全部处理完毕,
		// 下面开始处理容器中的所有BeanFactoryPostProcessor
		// Do not initialize FactoryBeans here: We need to leave all regular beans
		// uninitialized to let the bean factory post-processors apply to them!
		// 8.找出所有实现BeanFactoryPostProcessor接口的类
		//获取实现了BeanFactoryPostProcessor接口的类，获取beanDefinition的名称
		String[] postProcessorNames =
				beanFactory.getBeanNamesForType(BeanFactoryPostProcessor.class, true, false);

		// Separate between BeanFactoryPostProcessors that implement PriorityOrdered,
		// Ordered, and the rest.
		// 用于存放实现了PriorityOrdered接口的BeanFactoryPostProcessor
		List<BeanFactoryPostProcessor> priorityOrderedPostProcessors = new ArrayList<>();
		// 用于存放实现了Ordered接口的BeanFactoryPostProcessor的beanName
		List<String> orderedPostProcessorNames = new ArrayList<>();
		// 用于存放普通BeanFactoryPostProcessor的beanName
		List<String> nonOrderedPostProcessorNames = new ArrayList<>();
		// 8.1 遍历postProcessorNames, 将BeanFactoryPostProcessor按实现PriorityOrdered、实现Ordered接口、普通三种区分开
		for (String ppName : postProcessorNames) {
			// 8.2 跳过已经执行过的，如果上面步骤已经调用过的BeanDefinition跳过
			if (processedBeans.contains(ppName)) {
				// skip - already processed in first phase above
			}
			//实现了PriorityOrdered接口的
			else if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
				// 8.3 添加实现了PriorityOrdered接口的BeanFactoryPostProcessor
				//实例化BeanFactoryPostProcessor并添加到priorityOrderedPostProcessors
				priorityOrderedPostProcessors.add(beanFactory.getBean(ppName, BeanFactoryPostProcessor.class));
			}
			//实现了Ordered接口的
			else if (beanFactory.isTypeMatch(ppName, Ordered.class)) {
				// 8.4 添加实现了Ordered接口的BeanFactoryPostProcessor的beanName
				orderedPostProcessorNames.add(ppName);
			} else {
				// 8.5 添加剩下的普通BeanFactoryPostProcessor的beanName
				nonOrderedPostProcessorNames.add(ppName);
			}
		}

		// 9.调用所有实现PriorityOrdered接口的BeanFactoryPostProcessor
		// 9.1 对priorityOrderedPostProcessors排序
		// First, invoke the BeanFactoryPostProcessors that implement PriorityOrdered.
		sortPostProcessors(priorityOrderedPostProcessors, beanFactory);
		// 9.2 遍历priorityOrderedPostProcessors, 执行postProcessBeanFactory方法
		invokeBeanFactoryPostProcessors(priorityOrderedPostProcessors, beanFactory);

		// 10.调用所有实现Ordered接口的BeanFactoryPostProcessor
		// Next, invoke the BeanFactoryPostProcessors that implement Ordered.
		List<BeanFactoryPostProcessor> orderedPostProcessors = new ArrayList<>(orderedPostProcessorNames.size());

		//将所有实现了Ordered接口的类加入到orderedPostProcessors用来排序
		for (String postProcessorName : orderedPostProcessorNames) {
			// 10.1 获取postProcessorName对应的bean实例, 添加到orderedPostProcessors, 准备执行
			orderedPostProcessors.add(beanFactory.getBean(postProcessorName, BeanFactoryPostProcessor.class));
		}

		// 10.2 对orderedPostProcessors排序
		sortPostProcessors(orderedPostProcessors, beanFactory);
		// 10.3 遍历orderedPostProcessors, 执行postProcessBeanFactory方法
		invokeBeanFactoryPostProcessors(orderedPostProcessors, beanFactory);

		// 11.调用所有剩下的BeanFactoryPostProcessor
		//没有实现任何排序接口的BeanFactoryPostProcessor操作
		// Finally, invoke all other BeanFactoryPostProcessors.
		List<BeanFactoryPostProcessor> nonOrderedPostProcessors = new ArrayList<>(nonOrderedPostProcessorNames.size());
		//遍历并添加到nonOrderedPostProcessors
		for (String postProcessorName : nonOrderedPostProcessorNames) {
			// 11.1 获取postProcessorName对应的bean实例, 添加到nonOrderedPostProcessors, 准备执行
			nonOrderedPostProcessors.add(beanFactory.getBean(postProcessorName, BeanFactoryPostProcessor.class));
		}
		// 11.2 遍历nonOrderedPostProcessors, 执行postProcessBeanFactory方法
		invokeBeanFactoryPostProcessors(nonOrderedPostProcessors, beanFactory);

		// Clear cached merged bean definitions since the post-processors might have
		// modified the original metadata, e.g. replacing placeholders in values...
		// 12.清除元数据缓存（mergedBeanDefinitions、allBeanNamesByType、singletonBeanNamesByType），
		// 因为后处理器可能已经修改了原始元数据，例如， 替换值中的占位符...
		beanFactory.clearMetadataCache();
	}

	/**
	 * 完成注册BeanPostProcessor
	 *
	 * @param beanFactory
	 * @param applicationContext
	 */
	public static void registerBeanPostProcessors(
			ConfigurableListableBeanFactory beanFactory, AbstractApplicationContext applicationContext) {
		//获取所有的BeanPostProcessor的BeanDefinition的name
		String[] postProcessorNames = beanFactory.getBeanNamesForType(BeanPostProcessor.class, true, false);

		// Register BeanPostProcessorChecker that logs an info message when
		// a bean is created during BeanPostProcessor instantiation, i.e. when
		// a bean is not eligible for getting processed by all BeanPostProcessors.
		// BeanPostProcessor的目标计数
		int beanProcessorTargetCount = beanFactory.getBeanPostProcessorCount() + 1 + postProcessorNames.length;
		// 2.添加BeanPostProcessorChecker(主要用于记录信息)到beanFactory中
		beanFactory.addBeanPostProcessor(new BeanPostProcessorChecker(beanFactory, beanProcessorTargetCount));

		// Separate between BeanPostProcessors that implement PriorityOrdered,
		// Ordered, and the rest.
		// 3.定义不同的变量用于区分: 实现PriorityOrdered接口的BeanPostProcessor、实现Ordered接口的BeanPostProcessor、普通BeanPostProcessor
		// 3.1 priorityOrderedPostProcessors: 用于存放实现PriorityOrdered接口的BeanPostProcessor
		List<BeanPostProcessor> priorityOrderedPostProcessors = new ArrayList<>();
		// 3.2 internalPostProcessors: 用于存放Spring内部的BeanPostProcessor
		List<BeanPostProcessor> internalPostProcessors = new ArrayList<>();
		// 3.3 orderedPostProcessorNames: 用于存放实现Ordered接口的BeanPostProcessor的beanName
		List<String> orderedPostProcessorNames = new ArrayList<>();
		// 3.4 nonOrderedPostProcessorNames: 用于存放普通BeanPostProcessor的beanName
		List<String> nonOrderedPostProcessorNames = new ArrayList<>();
		//提前实例化BeanPostProcessor类型的bean，然后bean进行排序
		// 4.遍历postProcessorNames, 将BeanPostProcessors按3.1 - 3.4定义的变量区分开
		for (String ppName : postProcessorNames) {
			// 4.1 如果ppName对应的Bean实例实现了PriorityOrdered接口, 则拿到ppName对应的Bean实例并添加到priorityOrderedPostProcessors
			if (beanFactory.isTypeMatch(ppName, PriorityOrdered.class)) {
				//getBean是实例化方法，后面我们在讲bean实例化过程是会着重讲到
				BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
				//添加到priorityOrderedPostProcessors集合
				priorityOrderedPostProcessors.add(pp);

				//判断类型是否是MergedBeanDefinitionPostProcessor，如果是则代码是内部使用的
				if (pp instanceof MergedBeanDefinitionPostProcessor) {
					// 4.2 如果ppName对应的Bean实例也实现了MergedBeanDefinitionPostProcessor接口,
					// 则将ppName对应的Bean实例添加到internalPostProcessors
					internalPostProcessors.add(pp);
				}
			} else if (beanFactory.isTypeMatch(ppName, Ordered.class)) {
				// 4.3 如果ppName对应的Bean实例没有实现PriorityOrdered接口, 但是实现了Ordered接口, 则将ppName添加到orderedPostProcessorNames
				orderedPostProcessorNames.add(ppName);
			} else {
				// 4.4 否则, 将ppName添加到nonOrderedPostProcessorNames
				nonOrderedPostProcessorNames.add(ppName);
			}
		}


		// First, register the BeanPostProcessors that implement PriorityOrdered.
		// 5.首先, 注册实现PriorityOrdered接口的BeanPostProcessors
		// 5.1 对priorityOrderedPostProcessors进行排序
		sortPostProcessors(priorityOrderedPostProcessors, beanFactory);

		// 5.2 注册priorityOrderedPostProcessors
		registerBeanPostProcessors(beanFactory, priorityOrderedPostProcessors);

		// Next, register the BeanPostProcessors that implement Ordered.
		// 6.接下来, 注册实现Ordered接口的BeanPostProcessors
		List<BeanPostProcessor> orderedPostProcessors = new ArrayList<>(orderedPostProcessorNames.size());
		//遍历实现了Ordered接口的orderedPostProcessorNames
		for (String ppName : orderedPostProcessorNames) {
			// 6.1 拿到ppName对应的BeanPostProcessor实例对象
			BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
			// 6.2 将ppName对应的BeanPostProcessor实例对象添加到orderedPostProcessors, 准备执行注册
			orderedPostProcessors.add(pp);
			//判断类型是否是MergedBeanDefinitionPostProcessor，如果是则代码是内部使用的
			if (pp instanceof MergedBeanDefinitionPostProcessor) {
				// 6.3 如果ppName对应的Bean实例也实现了MergedBeanDefinitionPostProcessor接口,
				// 则将ppName对应的Bean实例添加到internalPostProcessors
				internalPostProcessors.add(pp);
			}
		}
		// 6.4 对orderedPostProcessors进行排序
		sortPostProcessors(orderedPostProcessors, beanFactory);
		// 6.5 注册orderedPostProcessors
		registerBeanPostProcessors(beanFactory, orderedPostProcessors);

		//完成没有实现任何排序接口的BeanPostProcessor的注册
		// 7.注册所有常规的BeanPostProcessors（过程与6类似）
		// Now, register all regular BeanPostProcessors.
		List<BeanPostProcessor> nonOrderedPostProcessors = new ArrayList<>(nonOrderedPostProcessorNames.size());
		//遍历没有实现排序接口的nonOrderedPostProcessorNames
		for (String ppName : nonOrderedPostProcessorNames) {
			//实例化BeanPostProcessor
			BeanPostProcessor pp = beanFactory.getBean(ppName, BeanPostProcessor.class);
			//添加到nonOrderedPostProcessors
			nonOrderedPostProcessors.add(pp);
			//判断类型是否是MergedBeanDefinitionPostProcessor，如果是则代码是内部使用的
			if (pp instanceof MergedBeanDefinitionPostProcessor) {
				//添加到internalPostProcessors
				internalPostProcessors.add(pp);
			}
		}
		//注册到BeanFactory中
		registerBeanPostProcessors(beanFactory, nonOrderedPostProcessors);

		// 8.最后, 重新注册所有内部BeanPostProcessors（相当于内部的BeanPostProcessor会被移到处理器链的末尾）
		// 8.1 对internalPostProcessors进行排序
		// Finally, re-register all internal BeanPostProcessors.
		sortPostProcessors(internalPostProcessors, beanFactory);
		// 8.2注册internalPostProcessors
		registerBeanPostProcessors(beanFactory, internalPostProcessors);

		// 9.重新注册ApplicationListenerDetector（跟8类似，主要是为了移动到处理器链的末尾）
		// Re-register post-processor for detecting inner beans as ApplicationListeners,
		// moving it to the end of the processor chain (for picking up proxies etc).
		beanFactory.addBeanPostProcessor(new ApplicationListenerDetector(applicationContext));
	}

	/**
	 * 进行排序操作 越小越优先
	 *
	 * @param postProcessors
	 * @param beanFactory
	 */
	private static void sortPostProcessors(List<?> postProcessors, ConfigurableListableBeanFactory beanFactory) {
		Comparator<Object> comparatorToUse = null;
		if (beanFactory instanceof DefaultListableBeanFactory) {
			// 1.获取设置的比较器
			comparatorToUse = ((DefaultListableBeanFactory) beanFactory).getDependencyComparator();
		}
		if (comparatorToUse == null) {
			// 2.如果没有设置比较器, 则使用默认的OrderComparator
			comparatorToUse = OrderComparator.INSTANCE;
		}
		// 3.使用比较器对postProcessors进行排序
		postProcessors.sort(comparatorToUse);
	}

	/**
	 * Invoke the given BeanDefinitionRegistryPostProcessor beans.
	 * 调用实现了BeanDefinitionRegistryPostProcessor接口的bean，完成对注册的BeanDefinition的修改
	 */
	private static void invokeBeanDefinitionRegistryPostProcessors(
			Collection<? extends BeanDefinitionRegistryPostProcessor> postProcessors, BeanDefinitionRegistry registry) {
		//遍历调用BeanDefinitionRegistryPostProcessor
		for (BeanDefinitionRegistryPostProcessor postProcessor : postProcessors) {
			//完成调用过程
			postProcessor.postProcessBeanDefinitionRegistry(registry);
		}
	}

	/**
	 * Invoke the given BeanFactoryPostProcessor beans.
	 * <p>
	 * 调用BeanFactoryPostProcessor的postProcessBeanFactory方法同样可以完成BeanDefinition的新增和修改
	 */
	private static void invokeBeanFactoryPostProcessors(
			Collection<? extends BeanFactoryPostProcessor> postProcessors, ConfigurableListableBeanFactory beanFactory) {
		//遍历调用BeanFactoryPostProcessor
		for (BeanFactoryPostProcessor postProcessor : postProcessors) {
			//完成调用过程
			postProcessor.postProcessBeanFactory(beanFactory);
		}
	}

	/**
	 * Register the given BeanPostProcessor beans.
	 * 将BeanPostProcessor注册到beanFactory
	 */
	private static void registerBeanPostProcessors(
			ConfigurableListableBeanFactory beanFactory, List<BeanPostProcessor> postProcessors) {
		//1. 遍历所有的BeanPostProcessor
		for (BeanPostProcessor postProcessor : postProcessors) {
			// 2.将PostProcessor添加到BeanFactory中的beanPostProcessors缓存
			beanFactory.addBeanPostProcessor(postProcessor);
		}
	}


	/**
	 * BeanPostProcessor that logs an info message when a bean is created during
	 * BeanPostProcessor instantiation, i.e. when a bean is not eligible for
	 * getting processed by all BeanPostProcessors.
	 */
	private static final class BeanPostProcessorChecker implements BeanPostProcessor {

		private static final Log logger = LogFactory.getLog(BeanPostProcessorChecker.class);

		private final ConfigurableListableBeanFactory beanFactory;

		private final int beanPostProcessorTargetCount;

		public BeanPostProcessorChecker(ConfigurableListableBeanFactory beanFactory, int beanPostProcessorTargetCount) {
			this.beanFactory = beanFactory;
			this.beanPostProcessorTargetCount = beanPostProcessorTargetCount;
		}

		@Override
		public Object postProcessBeforeInitialization(Object bean, String beanName) {
			return bean;
		}

		@Override
		public Object postProcessAfterInitialization(Object bean, String beanName) {
			if (!(bean instanceof BeanPostProcessor) && !isInfrastructureBean(beanName) &&
					this.beanFactory.getBeanPostProcessorCount() < this.beanPostProcessorTargetCount) {
				if (logger.isInfoEnabled()) {
					logger.info("Bean '" + beanName + "' of type [" + bean.getClass().getName() +
							"] is not eligible for getting processed by all BeanPostProcessors " +
							"(for example: not eligible for auto-proxying)");
				}
			}
			return bean;
		}

		private boolean isInfrastructureBean(@Nullable String beanName) {
			if (beanName != null && this.beanFactory.containsBeanDefinition(beanName)) {
				BeanDefinition bd = this.beanFactory.getBeanDefinition(beanName);
				return (bd.getRole() == RootBeanDefinition.ROLE_INFRASTRUCTURE);
			}
			return false;
		}
	}

}
