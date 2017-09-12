module JL_BHFunction
    use JL_Mod_BHCommon
    use JL_Mod_FileIO
    contains
    
    !����ؼ���Ϊγ��(y1,y2)�ϵľ���ƽ��ֵ
    !���룺
    !  hgt:λ�Ƹ߶Ƚṹ��
    !  y1,y2��γ������
    !�����
    !   LongAvg:LongAvg(144)����(����144�ɷֱ���ȷ����ֵ360/2.5),����ƽ��ֵ
    subroutine calc_longitude_avg(hgt,y1,y2,LongAvg)
        type(hgt_t)::hgt
        real::y1,y2
        real::LongAvg(int(longitude_max/hgt%block_height))
        integer::iy1,iy2
        integer::i,j
        
        iy1=hgt_latitude_index(hgt,y1)
        iy2=hgt_latitude_index(hgt,y2)
        LongAvg=0.0
        
        do j=iy1,iy2
            do i=1,longitude_max/hgt%block_height
                LongAvg(i)=LongAvg(i)+hgt%H(j,i)
            end do
        end do
        LongAvg=LongAvg/(iy2-iy1+1)
    end subroutine
    
    !����ؼ���Ϊ����(x1,x2)�ϵ�γ��ƽ��ֵ
    !���룺
    !  hgt:λ�Ƹ߶Ƚṹ��
    !  x1,x2����������
    !�����
    !   LatAvg:LatAvg(73)����(����144�ɷֱ���ȷ����ֵ180/2.5+1),γ��ƽ��ֵ
    subroutine calc_latitude_avg(hgt,x1,x2,LatAvg)
        type(hgt_t)::hgt
        real::x1,x2
        real::LatAvg(int(latitude_max/hgt%block_width+1))
        integer::ix1,ix2
        integer::i,j
        
        ix1=hgt_longitude_index(hgt,x1)
        ix2=hgt_longitude_index(hgt,x2)
        LatAvg=0.0
        
        do j=1,int(latitude_max/hgt%block_width+1)
            do i=ix1,ix2
                LatAvg(j)=LatAvg(j)+hgt%H(j,i)
            enddo
            LatAvg(j)=LatAvg(j)/(ix2-ix1+1)            
        enddo
    end subroutine
    
    !����ĳһ�����ڵĻ���ƽ��ֵ
    !���룺
        !year,month,day:������
        !hgt,λ�Ƹ߶Ƚṹ��
    !�����
        !SHAvg(73,144):����ƽ��ֵ��ͨ���βδ��ء���������ֵ��Դ���ݷֱ��йأ��˴�Ϊ2.5*2.5��
    !subroutine Slide_Avg(hgt,year,month,day,SHAvg)
    subroutine Slide_Avg(DateS,HgtS,DataFilePath,SHAvg)
        !integer::year,month,day
        !type(hgt_t)::hgt
        type(DateStruct)::DateS
        type(HgtStruct)::Hgts
        character(len=200)::DataFilePath
        real::SHAvg(HgtS%widthIndex,HgtS%heightIndex)
        
        !real::SHAvg(latitude_max/hgt%block_width+1,longitude_max/hgt%block_height)
        real::H_temp(HgtS%widthIndex,HgtS%heightIndex)        
        integer::y_t,m_t,d_t !��ʱ��������
        integer::i,j,k
        integer::ix,iy
        iy=HgtS%widthIndex
        ix=HgtS%heightIndex
        SHAvg=0.0
      
        call Prev_day(DateS%year,DateS%month,DateS%day,y_t,m_t,d_t)
        call Prev_day(y_t,m_t,d_t,y_t,m_t,d_t)
        !ReadHgt(DateS,HgtS,DataFilePath,H)
        do i=1,5
            !call ReadHgt(H_temp,y_t,m_t,d_t)            
            call ReadHgt(DateS,HgtS,DataFilePath,H_temp)
            SHAvg(1:iy,1:ix)=SHAvg(1:iy,1:ix)+H_temp(1:iy,1:ix)
            call Next_day(y_t,m_t,d_t,y_t,m_t,d_t)
        enddo
        SHAvg=SHAvg/5
              
    end subroutine Slide_Avg
    
    !����������ѹλ���ݶ�
    !���룺
    !   hgt:λ�ƽṹ��
    !   x1,x2,y1,y2����γ�ȷ�Χ
    !   year,month,day��������
    !   step:γ�ȼ���ƫ��(�����½��ѹʱĬ��2.5 ����Ĭ��5.0)
    !   m:	γ���±���(�����½��ѹʱĬ��4 ����Ĭ��8)
    !   s_min:GHGS�½�
    !   n_max:GHGN�Ͻ�
    !���:G,λ���ݶ�
    subroutine calc_Block_Gradient(hgt,year,month,day,x1,x2,y1,y2,step, m, s_min, n_max,SHAvg,G)
        implicit none    
        type(hgt_t)::hgt
        integer::year,month,day
        real::x1,x2,y1,y2
        real::step,s_min,n_max
        integer::m
        real::G(:),SHAvg(:,:)
        
        real::GHGS,GHGN !�Ϸ�������λ�Ƹ߶��ݶ�
        integer::ix1,ix2,iy1,iy2,i,j
!        real::SHAvg(latitude_max/hgt%block_width+1,longitude_max/hgt%block_height)
        
        !����γ��ת��Ϊ�����±�
        ix1=hgt_longitude_index(hgt, x1)
        ix2=hgt_longitude_index(hgt, x2)
        iy1=hgt_latitude_index(hgt,y1)
        iy2=hgt_latitude_index(hgt,y2)
        
        G=32767.0
       
        !call Slide_Avg(hgt,year,month,day,SHAvg)!���㻬��ƽ��ֵ
        do i=ix1,ix2
            do j=iy1,iy2,int(step/hgt%block_width)
                if(j+2*m>latitude_max/hgt%block_width+1)then
                    write(*,*)"����������ѹλ���ݶ�ʱγ��Խ�磬γ��ֵ���ܴ���50N"
                    exit
                endif
                GHGS=(SHAvg(j+m,i)-SHAvg(j,i))/(m*hgt%block_height)
                GHGN=(SHAvg(j+2*m,i)-SHAvg(j+m,i))/(m*hgt%block_height)
                !write(*,*)GHGS,GHGN
                if(GHGS>s_min .and. GHGN< n_max) then
                    if(G(i-ix1+1) .EQ. 32767) then
                        G(i-ix1+1)=GHGS                    
                    else
                        G(i-ix1+1)=max(GHGS,G(i-ix1+1))
                    endif
                endif
            enddo
        enddo       
    end subroutine

end module JL_BHFunction